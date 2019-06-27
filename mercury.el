;;; mercury.el --- Emacs interface to FB Messenger. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 30 Apr 2019
;; Homepage: https://github.com/raxod502/mercury
;; Keywords: applications
;; Package-Requires: ((emacs "25.2"))
;; Version: 0

;;; Commentary:

;; TODO

;; Please see <https://github.com/raxod502/mercury> for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup mercury nil
  "Emacs interface to Facebook Messenger."
  :group 'applications
  :prefix "mercury-")

(defcustom mercury-directory
  (expand-file-name
   "mercury"
   (expand-file-name
    "var"
    user-emacs-directory))
  "Directory in which Mercury files are stored.
These include your session cookies, your messages history, and
the Mercury Python virtualenv."
  :type 'directory)

(defvar mercury--source-dir
  (file-name-directory
   (file-truename (or load-file-name buffer-file-name)))
  "Directory containing the Mercury source repository.")

(defun mercury--expand-file-name (&rest names)
  "Run `expand-file-name' on each of NAMES, starting from `mercury-directory'.
Example:

        (mercury--expand-file-name \"foo\" \"bar\")
  ;; => \"/home/you/.emacs.d/var/mercury/foo/bar\""
  (let ((dir mercury-directory))
    (dolist (name names)
      (setq dir (expand-file-name name dir)))
    dir))

(defvar mercury--server-buffer-name " *mercury-server*"
  "Name of buffer to use for Mercury server.")

(defmacro mercury--server-with-buffer (&rest body)
  "Exec BODY with the Mercury server buffer current.
Disable read-only status and go to the end of the buffer for the
duration of BODY."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create mercury--server-buffer-name)
     (special-mode)
     (save-excursion
       (let ((inhibit-read-only t))
         ,@body))))

(defun mercury--prepend-path (val path)
  "Prepend VAL to colon-delimited PATH."
  (if path
      (concat val ":" path)
    val))

(defmacro mercury--server-with-env (&rest body)
  "Exec BODY in context of Mercury virtualenv."
  (declare (indent defun))
  `(let* ((virtualenv-bin (mercury--expand-file-name "virtualenv" "bin"))
          (process-environment
           (cl-list*
            (concat "PATH=" (mercury--prepend-path
                             virtualenv-bin (getenv "PATH")))
            (concat "PYTHONPATH=" (mercury--prepend-path
                                   mercury--source-dir (getenv "PYTHONPATH")))))
          (exec-path (cons virtualenv-bin exec-path)))
     ,@body))

(defun mercury--server-install ()
  "If missing or outdated, install Mercury into a virtualenv."
  (let* ((virtualenv (mercury--expand-file-name "virtualenv"))
         (repo-lockfile
          (with-temp-buffer
            (insert-file-contents-literally
             (mercury--expand-file-name mercury--source-dir "poetry.lock"))
            (buffer-string)))
         (installed-lockfile
          (with-temp-buffer
            (ignore-errors
              (insert-file-contents-literally
               (mercury--expand-file-name virtualenv "poetry.lock"))
              (buffer-string)))))
    (unless (and (equal repo-lockfile installed-lockfile)
                 (mercury--server-with-buffer
                   (mercury--server-with-env
                     (= 0 (call-process
                           "python" nil t nil
                           "-m" "mercury" "--no-load-session")))))
      (mercury--server-with-buffer
        (ignore-errors
          (delete-directory virtualenv 'recursive))
        (make-directory mercury-directory 'parents)
        (unless (= 0 (call-process "python3" nil t nil
                                   "-m" "venv" virtualenv))
          (error "Failed to create virtualenv"))
        (mercury--server-with-env
          (let ((requirements nil))
            (with-temp-buffer
              (insert repo-lockfile)
              (goto-char (point-min))
              (while (re-search-forward
                      (concat
                       "name = \"\\(.+?\\)\""
                       "[[:ascii:][:nonascii:]]+?"
                       "version = \"\\(.+?\\)\"")
                      nil 'noerror)
                (push
                 (format "%s==%s" (match-string 1) (match-string 2))
                 requirements)))
            (unless (= 0 (apply
                          #'call-process
                          "pip" nil t nil
                          (cons "install" requirements)))
              (error "Failed to install dependencies")))
          (unless (= 0 (call-process
                        "python" nil t nil "-m" "mercury" "--no-load-session"))
            (error "Test run failed"))
          (copy-file
           (mercury--expand-file-name mercury--source-dir "poetry.lock")
           (mercury--expand-file-name virtualenv "poetry.lock")))))))

(defvar mercury--server-output ""
  "Collected output from stdout of the Mercury server.
This is used by the filter function to keep track of output in
the case that it does not receive a full line of output in a
single call.")

(defvar mercury--server-handlers nil
  "Alist of handlers for messages from the Mercury server.
Only the values are significant for the server; they are the
handlers. When the server sends a message, it is decoded from
JSON into an alist, and then the resulting object is passed to
each handler in turn until one of them returns non-nil.")

(defun mercury--server-filter (proc string)
  "Process filter for the Mercury server.
PROC is the Mercury server process, and STRING is the data that
was sent to stdout by the server."
  (setq mercury--server-output (concat mercury--server-output string))
  (mercury--with-server-buffer
    (while (string-match "\\(.*\\)\n" mercury--server-output)
      (let ((line (match-string 1 mercury--server-output)))
        (setq mercury--server-output (substring mercury--server-output (match-end 0)))
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (let ((inhibit-read-only t))
              (when (string-prefix-p "{" line)
                (insert "-> "))
              (insert line "\n"))
            (set-marker (process-mark proc) (point)))
          (when moving
            (goto-char (process-mark proc))))
        (when (string-prefix-p "{" line)
          (let ((resp (json-read-from-string line)))
            (when-let ((err (alist-get 'error resp)))
              (error "Mercury server error: %s" err))
            (cl-dolist (cell mercury--server-handlers)
              (when (funcall (cdr cell) resp)
                (cl-return)))))))))

(defvar mercury--server-process nil
  "Mercury server process object. Communicates on stdio.")

(defun mercury--server-stop ()
  "Stop the Mercury server, if it is running."
  (when (process-live-p mercury--server-process)
    (process-send-eof mercury--server-process)
    (accept-process-output mercury--server-process 0.1 nil 'just-this-one)
    (when (process-live-p mercury--server-process)
      (interrupt-process mercury--server-process)
      (accept-process-output mercury--server-process 0.1 nil 'just-this-one)
      (when (process-live-p mercury--server-process)
        (kill-process mercury--server-process)))))

(defun mercury--server-start ()
  "Start the Mercury server, if it is not already running."
  (unless (process-live-p mercury--server-process)
    (mercury--server-install)
    (mercury--server-with-buffer
      (mercury--server-with-env
        (setq mercury--server-process
              (make-process
               :name "mercury"
               :buffer (current-buffer)
               :command '("python" "-m" "mercury")
               :noquery t
               :filter #'mercury--server-filter))))))

(defun mercury--server-send-message (msg)
  "Send a MSG to the server.
MSG is converted to JSON before being fed to the server on stdin.
If the server has not been started, then start it first."
  (mercury--server-start)
  (let ((line (json-encode msg)))
    (mercury--server-with-buffer
      (let ((moving (= (point) (process-mark mercury--server-process))))
        (save-excursion
          (goto-char (process-mark mercury--server-process))
          (insert "<- " line "\n")
          (set-marker (process-mark mercury--server-process) (point)))
        (when moving
          (goto-char (process-mark mercury--server-process)))
        (process-send-string mercury--server-process line)
        (process-send-string mercury--server-process "\n")))))

(defvar mercury--message-counter 0
  "Value used for the `id' of the next message to the server.")

(defun mercury--server-get-response (msg callback)
  "Send a MSG to the server, and invoke CALLBACK with the response.
An `id' attribute is added automatically to the message,
overwriting any existing one. The CALLBACK is invoked with the
same buffer current as is current when this function is invoked."
  (let ((id mercury--message-counter)
        (buffer (current-buffer)))
    (cl-incf mercury--message-counter)
    (setf (alist-get id mercury--server-handlers)
          (lambda (resp)
            (when (eq id (alist-get 'id resp))
              (prog1 t
                (setf (alist-get id mercury--server-handlers nil 'remove) nil)
                (with-current-buffer (if (buffer-live-p buffer)
                                         buffer
                                       (current-buffer))
                  (funcall callback resp))))))
    (setf (alist-get 'id msg) id)
    (mercury--server-send-message msg)))

(defcustom mercury-thread-list-block-size 20
  "How many threads to fetch at a time for the thread list buffer.
The default value matches the number of threads that can be
fetched from Facebook Messenger with a single API call."
  :type 'integer)

(defvar-local mercury--thread-list nil
  "Data for the thread list displayed in the current buffer.
The format is as returned by the Mercury server.")

(defun mercury--thread-list-redisplay ()
  "Recompute the text in the current (thread list) buffer.
This function uses the value of `mercury--thread-list'."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (column (current-column)))
    (erase-buffer)
    (seq-do (lambda (thread)
              (insert (alist-get 'name thread) "\n"))
            (reverse mercury--thread-list))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column))
  (message "Refreshing...done"))

(defun mercury-thread-list-refresh (&optional more)
  "Reset the current buffer to match the Mercury server's thread list.
With prefix arg MORE, display more threads than are currently
shown, according to the value of
`mercury-thread-list-block-size'."
  (interactive "P")
  (unless (derived-mode-p 'mercury-thread-list-mode)
    (user-error "Not in a Mercury thread list buffer"))
  (message "Refreshing...")
  (mercury--server-get-response
   `((message . getThreads)
     (numThreads . ,(+ mercury-thread-list-block-size
                       (if more (length mercury--thread-list) 0))))
   (lambda (resp)
     (setq mercury--thread-list (alist-get 'threads resp))
     (mercury--thread-list-redisplay))))

(define-derived-mode mercury-thread-list-mode special-mode "Mercury"
  "Major mode to list Mercury threads.")

(defcustom mercury-thread-list-keys
  '(("g" . mercury-thread-list-refresh))
  "Alist of keys for `mercury-thread-list-mode-map'.
You must set this variable before loading Mercury in order for
your setting to take effect. The keys are strings for `kbd', and
the values are functions."
  :type '(alist :key-type string :value-type function))

(map-apply (lambda (keys func)
             (define-key mercury-thread-list-mode-map (kbd keys) func))
           mercury-thread-list-keys)

(defcustom mercury-thread-list-buffer-name "*mercury-threads*"
  "Name for Mercury thread list buffer."
  :type 'string)

(defun mercury-thread-list ()
  "Pop to Mercury thread list buffer, creating it if necessary."
  (interactive)
  (with-current-buffer (get-buffer-create mercury-thread-list-buffer-name)
    (mercury-thread-list-mode)
    (pop-to-buffer (current-buffer))))

;;;; Closing remarks

(provide 'mercury)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; mercury.el ends here
