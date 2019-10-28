# Mercury

Mercury is an [Emacs](https://www.gnu.org/software/emacs/) interface
to [Facebook Messenger](https://www.messenger.com/), SMS (via
[Pushbullet](https://www.pushbullet.com/)), and
[Signal](https://signal.org/).

It is currently under construction, and hence does not actually
provide these features yet.

## Message-passing API

Communication between the Emacs Lisp frontend and the Python backend
is done by message passing on stdio. Each message is a single line
containing a JSON object.

General message protocol:

* map:
    * `type`: string, message type identifier
    * `id`: string, identifier to match server responses to their
      original messages
    * `data`: map, contents dependent on message type
    * `error`: string, error message (or null if request successful)
        * notes:
            * only used for response messages

Request-response messages:

* `addAccount`:
    * message:
        * `service`: string, must be `messenger`
        * `name`: string, human-readable name for the account
    * response:
        * `id`: string, account ID
* `removeAccount`:
    * message:
        * `id`: string, account ID
* `getAccounts`:
    * response:
        * map:
            * keys: strings, account IDs
            * values: maps
                * `service`: string, always `messenger`
                * `name`: string, human-readable name for the account
                * `loginRequired`: boolean
                * `loginFields`: list
                    * values: maps
                        * `field`: string, internal identifier used in
                          response
                        * `name`: string, display name
                        * `private`: boolean, whether this field
                          should be treated as a password
* `login`:
    * message:
        * `aid`: string, account ID
        * `fields`: map
            * keys: strings, as in `field` key under `getAccounts`
              response
            * values: strings, as provided by the user
* `logout`:
    * message:
        * `aid`: string, account ID
* `getConversations`:
    * message:
        * `aid`: string, account ID
        * `limit`: integer, how many conversations to retrieve (or
          null for default)
        * `offset`: integer, how many conversations to skip (or null
          or omitted for default, skipping no conversations)
    * response:
        * `conversations`: list, sorted by `timestamp` descending
            * values: maps
                * `id`: string, conversation ID
                * `name`: string, service-provided display name for
                  conversation
                * `timestamp`: integer, most recent message timestamp
                  (may be more recent than any message we have if we
                  haven't fetched the most recent message yet)
                * `participants`: list
                    * values: maps
                        * `id`: string, user ID
                        * `name`: string, display name
                        * `you`: boolean, whether you are this user
* `getMessages`:
    * message:
        * `aid`: string, account ID
        * `cid`: string, conversation ID
        * `limit`: integer, how many messages to retrieve (or null or
          omitted for default)
        * `offset`: integer, how many messages to skip (or null or
          omitted for default, skipping no messages)
    * response:
        * `messages`: list, sorted by `timestamp`
            * values: maps
                * `id`: string, message ID
                * `type`: string, either `text` or `image` or `file` or
                  `unsupported`
                * `content`: string, message contents for `text` or URL
                  for `image` and `file` or description for `unsupported`
                * `timestamp`: integer, time at which message was sent
                * `sender`: map
                    * `id`: string, user ID
                    * `name`: string, name
                    * `you`: boolean, whether you are this user
        * `participants`: list
            * values: maps
                * `id`: string, user ID
                * `name`: string, display name
                * `you`: boolean, whether you are this user
                * `lastSeenMessage`: string, message ID, or null if
                  the message has not been fetched yet
* `sendMessage`:
    * message:
        * `aid`: string, account ID
        * `cid`: string, conversation ID
        * `type`: string, either `text` or `image` or `file`
        * `content`: string, message contents for `text` or filename for
          `image` and `file`
    * response: empty

Notification messages:

* `resyncNeeded`: empty
* `receivedMessage`:
    * `id`: string, message ID
    * `type`: string, either `text` or `image` or `file` or
      `unsupported`
    * `content`: string, message contents for `text` or URL
      for `image` and `file` or description for `unsupported`
    * `timestamp`: integer, time at which message was sent
    * `sender`: map
        * `id`: string, user ID
        * `name`: string, name
        * `you`: boolean, whether you are this user
* `receivedReadReceipt`:
    * `id`: string, message ID
    * `conversation`: map
        * `id`: string, conversation ID
        * `name`: string, display name of conversation
    * `user`: map
        * `id`: string, user ID
        * `name`: string, display name of user

## Service client API

Any API method may throw `LoginRequiredError` upon authentication
failure or invalid session, or `ServiceError` upon bad data or
unexpected error from upstream API. Other errors should only be thrown
in case of programmer error and will result in the use of lethal force
by Mercury on the service client.

* `get_session()`:
    * returns: string, as used for `restore_session` (or null if no
      active session)
* `restore_session(session)`:
    * `session`: string, as returned by `get_session`
    * returns: nothing
    * notes:
        * should validate session and throw `LoginRequiredError` if
          it's not valid
* `get_login_fields()`:
    * returns: list
        * values: maps
            * `field`: string, internal identifier used in response
            * `name`: string, display name
            * `private`: boolean, whether this field should be treated
              as a password
* `login(fields)`:
    * `fields`: map
        * keys: strings, as returned by `get_login_fields` under
          `field`
        * values: strings, values filled in by the user
    * preconditions:
        * `logout()` has been called
* `logout()`:
    * returns: nothing
    * notes:
        * should silently do nothing if already logged out or there is
          no existing session
* `get_you()`:
    * returns: string, user ID of you
* `get_users(uids)`:
    * `uids`: list
        * values: strings, user IDs
    * returns: map
        * keys: strings, user IDs
        * values: maps
            * `name`: string, display name of user
* `get_conversations(before)`:
    * `before`: timestamp at which to start retrieving conversations
      (or null to retrieve most recently updated conversations)
    * returns: map
        * `conversations`: list, sorted by `timestamp` descending
            * length is unspecified but should only be zero if there
              are no matching conversations
            * values: maps
                * `id`: string, conversation ID
                * `name`: string, conversation display name
                * `timestamp`: integer, most recent message timestamp
                  (may be more recent than any message we have if we
                  haven't fetched the most recent message yet)
                * `participants`: map or omitted
                    * keys: strings, user IDs
                    * values: maps
                        * `lastSeenMessage`: string, message ID (or
                          null or omitted if there's no update to
                          report)
                * `messages`: list, sorted by `timestamp` descending,
                  or omitted
                    * values: maps
                        * `id`: string, message ID
                        * `type`: string, either `text` or `image` or
                          `file` or `unsupported`
                        * `content`: string, message contents for
                          `text` or URL for `image` and `file` or
                          description for `unsupported`
                        * `timestamp`: integer, time at which message
                          was sent
        * `users`: map or omitted
            * keys: strings, user IDs
            * values: maps
                * `name`: string, display name of user (or null or
                  omitted if this information is not available)
* `get_messages(before)`:
    * `before`: timestamp at which to start retrieving messages (or
      null to retrieve most recent messages)
    * returns: map
        * `messages`: list, sorted by `timestamp` descending
            * length is unspecified but should only be zero if there are
              no matching messages
            * values: maps
                * `id`: string, message ID
                * `type`: string, either `text` or `image` or `file` or
                  `unsupported`
                * `content`: string, message contents for `text` or URL
                  for `image` and `file` or description for `unsupported`
                * `timestamp`: integer, time at which message was sent
                * `sender`: string, user ID
        * `participants`: map or omitted
            * keys: strings, user IDs
                * notes:
                    * does not need to include all participants, only
                      ones for which data should be updated
            * values: maps
                * `lastSeenMessage`: string, message ID (or null or
                  omitted if there's no update to report)
        * `users`: map or omitted
            * keys: strings, user IDs
            * values: maps
                * `name`: string, display name of user (or null or
                  omitted if this information is not available)
* `send_message(cid, type, content)`:
    * `cid`: string, conversation ID
    * `type`: string, either `text` or `image` or `file`
    * `content`: string, message contents for `text` or filename for
      `image` and `file`
    * returns: nothing

## Store format

All timestamps are milliseconds since the epoch.

* `$MERCURY_SESSION_FILE` or `$XDG_CONFIG_HOME/mercury/sessions.json`:
  map
    * `sessions`: map
        * keys: strings, account IDs
        * values: JSON objects
* `$MERCURY_DATA_FILE` or `$XDG_CONFIG_HOME/mercury/data.json`: map
    * `version`: integer, schema version
    * `accounts`: map
        * keys: strings, account IDs
        * values: maps
            * `name`: string, account display name
            * `users`: map
                * keys: strings, user IDs
                * values: maps
                    * `name`: user display name
            * `conversations`: list, sorted by `timestamp` descending
                * values: maps
                    * `id`: string, conversation ID
                    * `name`: string, conversation display name
                    * `timestamp`: integer, most recent message
                      timestamp (may be more recent than any message
                      we have if we haven't fetched the most recent
                      message yet)
                    * `participants`: map
                        * keys: strings, user IDs
                        * values: maps
                            * `lastSeenMessage`: string, message ID
                              (or null if we haven't fetched the
                              message yet)
                * `messages`: list, sorted by `timestamp` descending
                    * values: maps
                        * `id`: string, message ID
                        * `type`: string, either `text` or `image` or
                          `file` or `unsupported`
                        * `content`: string, message contents for
                          `text` or URL for `image` and `file` or
                          description for `unsupported`
                        * `timestamp`: integer, time at which message
                          was sent
                        * `sender`: string, user ID
