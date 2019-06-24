;;; mercury.el --- Emacs interface to FB Messenger. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 30 Apr 2019
;; Homepage: https://github.com/raxod502/mercury
;; Keywords: applications
;; Package-Requires: ((emacs "26"))
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
the Mercury Python virtualenv.")

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

(defmacro mercury--with-server-buffer (&rest body)
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

(defmacro mercury--with-server-env (&rest body)
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
                 (mercury--with-server-buffer
                   (mercury--with-server-env
                     (= 0 (call-process
                           "python" nil t nil
                           "-m" "mercury" "--no-load-session")))))
      (mercury--with-server-buffer
        (ignore-errors
          (delete-directory virtualenv 'recursive))
        (make-directory mercury-directory 'parents)
        (unless (= 0 (call-process "python3" nil t nil
                                   "-m" "venv" virtualenv))
          (error "Failed to create virtualenv"))
        (mercury--with-server-env
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

(define-derived-mode mercury-thread-list-mode special-mode "Mercury"
  "Major mode to list Mercury threads.")

;;;; Closing remarks

(provide 'mercury)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; mercury.el ends here
