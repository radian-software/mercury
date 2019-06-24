import json
import pathlib

import atomicwrites
import fbchat
import fbchat.models


class Server:

    _SESSION_FILE = pathlib.Path("session.json")

    def _read_session_file(self):
        if not Server._SESSION_FILE.is_file():
            return
        with open(Server._SESSION_FILE) as f:
            contents = json.load(f)
            session_cookies = contents["session_cookies"]
            if self.client:
                self.client.logout()
            try:
                self.client = fbchat.Client(
                    None, None, session_cookies=session_cookies,
                )
            except fbchat.models.FBchatException:
                pass

    def _write_session_file(self):
        contents = {
            "session_cookies": self.client.getSession(),
        }
        with atomicwrites.atomic_write(Server._SESSION_FILE, overwrite=True) as f:
            json.dump(contents, f, indent=2)
            f.write("\n")

    def __init__(self, send_message):
        self.send_message = send_message
        self.client = None
        self._read_session_file()

    def _handle_message(self, message):
        message_type = message.get("message")
        if not message_type:
            raise Exception("no message type")
        if message_type == "login":
            username = message["username"]
            password = message["password"]
            if self.client:
                self.client.logout()
            self.client = fbchat.Client(username, password)
            return {}
        if not self.client.isLoggedIn():
            self.send_message({
                "message": "requestLogin",
            })
            raise Exception("not logged in")

    def handle_message(self, message):
        try:
            response = self._handle_message()
            self.send_message({
                "message": "result",
                "id": message.get("id"),
                "error": None,
                **response,
            })
        except Exception as e:
            self.send_message({
                "message": "result",
                "id": message.get("id"),
                "error": str(e),
            })
