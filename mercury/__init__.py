import json
import pathlib

import fbchat
import fbchat.models


SESSION_FILE = pathlib.Path("session.json")


def read_session_file():
    if not SESSION_FILE.is_file():
        return None
    with open(SESSION_FILE) as f:
        contents = json.load(f)
        return contents["session_cookies"]


def write_session_file(session_cookies):
    contents = {
        "session_cookies": session_cookies,
    }
    with open(SESSION_FILE, "w") as f:
        json.dump(contents, f, indent=2)
        f.write("\n")


class Server:

    def __init__(self, broadcast):
        self.broadcast = broadcast
        self.client = None
        session_cookies = read_session_file()
        if session_cookies:
            try:
                self.client = fbchat.Client(
                    None, None, session_cookies=session_cookies,
                )
            except fbchat.models.FBchatException:
                pass

    def _response(self, num, **kwargs):
        return {
            "num": num,
            "error": None,
            "login_needed": False,
            **kwargs
        }

    def _broadcast(self, **kwargs):
        self.broadcast({
            **kwargs,
        })

    def send_request(self, request):
        command = request["command"]
        num = request["num"]
        if command == "login":
            username = request["username"]
            password = request["password"]
            if self.client:
                self.client.logout()
            self.client = fbchat.Client(
                username, password,
            )
            write_session_file(self.client.getSession())
            return self._response(num)
        if not (self.client and self.client.isLoggedIn()):
            return self._response(
                num,
                error="not logged in",
                login_needed=True,
            )
        if command == "list_threads":
            limit = request["limit"]
            offset = request["offset"]
            # Fetch 20 threads at a time, as this is the maximum per request.
            threads = []
            while limit > 0:
                threads.extend(self.client.fetchThreadList(
                    offset=offset,
                    limit=min(limit, 20),
                ))
                offset += 20
                limit -= 20
            thread_info = []
            for thread in threads:
                thread_info.append({
                    "uid": thread.uid,
                    "name": thread.name,
                })
            return self._response(
                num,
                threads=thread_info,
            )
