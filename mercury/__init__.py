import json
import pathlib
import re
import sys
import traceback

import atomicwrites
import fbchat
import fbchat.models


class Client:

    def __init__(self):
        self.client = None

    def _log(self, name):
        print("fbchat: {}".format(name), file=sys.stderr)

    def login(self, username, password):
        self._log("login")
        if self.client is None:
            self.client = fbchat.Client(username, password)
        else:
            self.client.login(username, password)

    def setSession(self, session_cookies):
        self._log("setSession")
        if self.client is None:
            self.client = fbchat.Client(
                None, None, session_cookies=session_cookies,
            )
        else:
            self.client.setSession(session_cookies)

    def isLoggedIn(self):
        self._log("isLoggedIn")
        if self.client is None:
            return False
        return self.client.isLoggedIn()

    def __getattr__(self, name):
        orig = getattr(self.client, name)
        if not callable(orig):
            return orig

        if not self.client:
            raise Exception("not logged in")

        def wrapped(*args, **kwargs):
            self._log(name)
            return orig(*args, **kwargs)

        return wrapped


class ThreadList:

    _THREADS_FILE = pathlib.Path("threads.json")

    def _read_threads_file(self):
        if ThreadList._THREADS_FILE.is_file():
            with open(ThreadList._THREADS_FILE) as f:
                self.threads = json.load(f)

    def _write_threads_file(self):
        with atomicwrites.atomic_write(ThreadList._THREADS_FILE, overwrite=True) as f:
            json.dump(self.threads, f, indent=2)
            f.write("\n")

    def __init__(self):
        self.threads = []
        self._read_threads_file()

    def _convert_fb_thread(self, fb_thread):
        return {
            "threadID": fb_thread.uid,
            "name": fb_thread.name,
            "timestamp": int(fb_thread.last_message_timestamp),
            "unread": None,  # computed later
        }

    def _fetch(self, client, before=None):
        fb_threads = client.fetchThreadList(before=before)
        threads = [self._convert_fb_thread(fbt) for fbt in fb_threads]
        threads.reverse()
        return threads

    def _check_unread_statuses(self, client):
        unread_tids = set(client.fetchUnread())
        for thread in self.threads:
            thread["unread"] = thread["threadID"] in unread_tids

    def _fetch_latest(self, client):
        if not self.threads:
            self.threads = self._fetch(client)
        else:
            since_timestamp = self.threads[-1]["timestamp"]
            new_threads = self._fetch(client)
            # Fetch more threads until we have definitely fetched all
            # the threads more recently updated than the most recently
            # updated thread we already have.
            while new_threads[0]["timestamp"] > since_timestamp:
                new_thread_batch = self._fetch(
                    client, before=new_threads[0]["timestamp"],
                )
                new_threads = new_thread_batch + new_threads
            # Now discard the threads whose updates we already have.
            new_threads = [
                t for t in new_threads if t["timestamp"] > since_timestamp
            ]
            # Discard any new threads we already have an older copy of.
            new_thread_ids = {t["threadID"] for t in new_threads}
            filtered_threads = [
                t for t in self.threads if t["threadID"] not in new_thread_ids
            ]
            # Atomic update.
            self.threads = filtered_threads + new_threads

    def _fetch_earlier(self, client):
        old_threads = self._fetch(client, before=self.threads[0]["timestamp"])
        thread_ids = {t["threadID"] for t in self.threads}
        # In addition to threads that actually come before the
        # provided timestamp, the API also returns threads with the
        # same timestamp. Presumably it is possible for more than one
        # thread to have the same timestamp, although hopefully not
        # more than 20. Anyway, we should filter the ones we already
        # have.
        filtered_old_threads = [
            t for t in old_threads if t["threadID"] not in thread_ids
        ]
        # Atomic update.
        self.threads = filtered_old_threads + self.threads
        self._write_threads_file()

    def _finalize(self, client):
        self._check_unread_statuses(client)
        self._write_threads_file()

    def get_threads(self, client, num, before=None):
        assert num > 0, "negative thread count"
        if not before:
            self._fetch_latest(client)
        while True:
            if len(self.threads) >= num:
                if not before:
                    self._finalize(client)
                    return self.threads[-num:]
                if self.threads[num]["timestamp"] <= before:
                    offset = 0
                    while self.threads[offset + num]["timestamp"] < before:
                        offset += 1
                    self._finalize(client)
                    return self.threads[offset:offset + num]
            self._fetch_earlier(client)


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
        assert re.fullmatch(r"[0-9]+", self.tid), "illegal thread ID"
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
            try:
                self.client.setSession(session_cookies)
            except fbchat.models.FBchatException:
                pass

    def _write_session_file(self):
        contents = {
            "session_cookies": self.client.getSession(),
        }
        with atomicwrites.atomic_write(Server._SESSION_FILE, overwrite=True) as f:
            json.dump(contents, f, indent=2)
            f.write("\n")

    def __init__(self, send_message, load_session):
        self.send_message = send_message
        self.client = Client()
        if load_session:
            self._read_session_file()
        self.thread_list = ThreadList()

    def _handle_message(self, message):
        message_type = message.get("message")
        if not message_type:
            raise Exception("no message type")
        if message_type == "login":
            username = message["username"]
            password = message["password"]
            self.client.login(username, password)
            return {}
        if not self.client.isLoggedIn():
            self.send_message({
                "message": "requestLogin",
            })
            raise Exception("not logged in")
        if message_type == "getThreads":
            num = message["numThreads"]
            before = message.get("beforeTimestamp")
            return {
                "threads": self.thread_list.get_threads(
                    self.client, num, before=before,
                )
            }

    def handle_message(self, message):
        try:
            response = self._handle_message(message)
            self.send_message({
                "message": "result",
                "id": message.get("id"),
                "error": None,
                **response,
            })
        except Exception as e:
            traceback.print_exc()
            self.send_message({
                "message": "result",
                "id": message.get("id"),
                "error": "{}: {}".format(type(e).__name__, e),
            })
