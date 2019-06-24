import json
import pathlib
import re

import atomicwrites
import fbchat
import fbchat.models


class ThreadList:

    _THREADS_FILE = pathlib.Path("threads.json")

    def _read_threads_file(self):
        if ThreadList._THREADS_FILE.is_file():
            with open(ThreadList._THREADS_FILE) as f:
                self.threads = json.load(f)

    def _write_thread_file(self):
        with atomicwrites.atomic_write(ThreadList._THREADS_FILE, overwrite=True) as f:
            json.dump(self.threads, f, indent=2)
            f.write("\n")

    def __init__(self):
        self.threads = []
        self._read_threads_file()


class Thread:

    _THREADS_DIR = pathlib.Path("threads")

    def _get_thread_filename(self):
        return Thread._THREADS_DIR / (self.tid + ".json")

    def _read_thread_file(self):
        filename = self._get_thread_filename()
        if filename.is_file():
            with open(filename) as f:
                self.messages = json.load(f)

    def _write_thread_file(self):
        filename = self._get_thread_filename()
        filename.parent.mkdir(parents=True, exist_ok=True)
        with atomicwrites.atomic_write(filename, overwrite=True) as f:
            json.dump(self.messages, f, indent=2)
            f.write("\n")

    def __init__(self, tid):
        assert re.fullmatch(r"[0-9]+", self.tid)
        self.tid = tid
        self.messages = []
        self._read_messages_file()


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
