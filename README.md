# Mercury

Mercury is an [Emacs](https://www.gnu.org/software/emacs/) interface
to [Facebook Messenger](https://www.messenger.com/).

It is currently under construction.

## Server API

Communication between the Emacs Lisp frontend and the Python backend
is done by message passing on stdio. Each message is a single line
containing a JSON object.

### Request login

Sent by server if a `login` message is needed.

    {
      "message": "requestLogin"
    }

### Login

Sent by client to log in.

    {
      "command": "login",
      "username": "radon.neon@gmail.com",
      "password": "[REDACTED]",
      "id": "42"
    }

Receives response from server.

    {
      "command": "result",
      "id": "42",
      "error": null
    }

On error.

    {
      "command": "result",
      "id": 42,
      "error": "Couldn't log in for some reason"
    }

### Get threads

Sent by client to fetch the initial thread list.

    {
      "command": "getThreads",
      "id": "51",
      "numThreads": 30
    }

Receives response from server.

    {
      "command": "result",
      "id": "51",
      "error": null,
      "threads": [
        {
          "threadID": "1234",
          "timestamp": "1561149590498",
          "unread": true
        },
        {
          "threadID": "5678",
          "timestamp": "1561146263105",
          "unread": false
        }
      ]
    }

Sent by client to fetch more threads if desired.

    {
      "command": "getThreads",
      "id": "52",
      "numThreads": 30,
      "beforeTimestamp": "1561146263105"
    }

Receives same response.

### Notify about new message

Sent by server.

    {
      "message": "receiveMessage",
      "threadID": "1234",
      "senderID": "5678",
      "timestamp": "1561146263105",
      "type": "text",
      "text": "Hello, world!"
    }

For images.

    {
      "message": "receiveMessage",
      "threadID": "1234",
      "senderID": "5678",
      "timestamp": "1561146262063",
      "type": "image",
      "imageURL": "https://scontent-sjc3-1.xx.fbcdn.net/..."
    }

Same for files except the `type` is `file` and instead of `imageURL`
we have `fileURL`.

### Get messages

Sent by client to fetch the initial messages.

    {
      "message": "getMessages",
      "id": "77",
      "threadID": "1234",
      "numMessages": 30
    }

Response from server.

    {
      "message": "result",
      "id": "77",
      "error": null,
      "messages": [
        {
          "senderID": "1234",
          "timestamp": "1561146262063",
          "type": "text",
          "text": "Hi there!"
        }
      ]
    }

Sent by client to fetch more messages if desired.

    {
      "message": "getMessages",
      "id": "78",
      "threadID": "1234",
      "numMessages": 30,
      "beforeTimestamp": "1561146262063"
    }
