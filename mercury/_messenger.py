"""
Backend for Facebook Messenger <https://www.messenger.com/>.
"""

import functools
import json

import fbchat
from fbchat import FBchatException

from mercury import _api as api
from mercury._api import LoginRequiredError, ServiceError
from mercury._util import log


def natural_language_join(names):
    """
    Given ["x", "y", "z"], return "x, y, and z".
    """
    names = list(names)
    if len(names) == 0:
        raise ValueError("Nobody")
    elif len(names) == 1:
        return names[0]
    elif len(names) == 2:
        return names[0] + " and " + names[1]
    else:
        return ", ".join(names[:-1]) + ", and " + names[-1]


def wrap_fbchat(*, require_login):
    """
    Decorator that turns FBchatException into ServiceError and throws
    LoginRequiredError if there's no client initialized (unless
    require_login is false). There's currently no support for
    detecting if the session times out or we've been forcibly logged
    out and we get an auth error (although I'm not sure Messenger ever
    does time out sessions).
    """

    def decorator(fn):
        @functools.wraps(fn)
        def wrapped(self, *args, **kwargs):
            if require_login and self.client is None:
                raise LoginRequiredError
            try:
                return fn(self, *args, **kwargs)
            except fbchat.FBchatException as e:
                raise ServiceError from e

        return wrapped

    return decorator


class MessengerService(api.Service):
    """
    Backend for Facebook Messenger.
    """

    def __init__(self):
        self.client = None

    def get_session(self):
        if self.client is None:
            return None
        return json.dumps(self.client.getSession())

    @wrap_fbchat(require_login=False)
    def restore_session(self, session):
        try:
            log("fbchat: restoreSession(...)")
            self.client = fbchat.Client(None, None, session_cookies=json.loads(session))
        except (FBchatException, json.JSONDecodeError):
            raise LoginRequiredError

    def get_login_fields(self):
        return [
            {
                "field": "email",
                "name": "Email address",
                "identifiable": True,
                "private": False,
            },
            {
                "field": "password",
                "name": "Password",
                "identifiable": False,
                "private": True,
            },
        ]

    @wrap_fbchat(require_login=False)
    def login(self, fields):
        log("fbchat: login({}, ...)", repr(fields["email"]))
        self.client = fbchat.Client(fields["email"], fields["password"])

    @wrap_fbchat(require_login=False)
    def logout(self):
        if self.client is None:
            return
        log("fbchat: logout()")
        if not self.client.logout():
            raise ServiceError("failed to log out")
        self.client = None

    @wrap_fbchat(require_login=True)
    def get_you(self):
        return self.client.uid

    @wrap_fbchat(require_login=True)
    def get_users(self, uids):
        display_uids = list(uids)
        if len(uids) > 10:
            display_uids[10:] = ["..."]
        log("fbchat: fetchUserInfo({})", ", ".join(display_uids))
        results = {
            u.uid: {"name": u.name} for u in self.client.fetchUserInfo(*uids).values()
        }
        if set(results) != set(uids):
            raise ServiceError("got info back for wrong users")
        return results

    def _get_participants(self, fb_thread):
        if fb_thread.type == fbchat.ThreadType.GROUP:
            return fb_thread.participants
        elif fb_thread.type == fbchat.ThreadType.USER:
            return {self.get_you(), fb_thread.uid}
        else:
            return set()

    @wrap_fbchat(require_login=True)
    def get_conversations(self, before):
        log("fbchat: fetchThreadList(before={})", before)
        fb_threads = self.client.fetchThreadList(before=before)
        # Figure out what user IDs we have to fetch info for in order
        # to correctly assign thread names.
        all_uids = {
            p for t in fb_threads if not t.name for p in self._get_participants(t)
        }
        user_info = self.get_users(all_uids) if all_uids else {}
        threads = []
        for fb_thread in fb_threads:
            participants = self._get_participants(fb_thread)
            if not participants:
                continue
            name = fb_thread.name or natural_language_join(
                sorted(
                    user_info[p]["name"] for p in participants if p != self.client.uid
                )
            )
            threads.append(
                {
                    "id": fb_thread.uid,
                    "name": name,
                    "participants": {uid: {} for uid in participants},
                    "timestamp": int(fb_thread.last_message_timestamp),
                }
            )
        return {"conversations": threads, "users": user_info}

    def _convert_message(self, fb_message, user_info):
        base = {
            "id": fb_message.uid,
            "timestamp": int(fb_message.timestamp),
            "sender": {
                "id": fb_message.author,
                "name": user_info.get(
                    fb_message.author, "User ID " + str(fb_message.author)
                ),
            },
        }
        messages = []
        return messages

    @wrap_fbchat(require_login=True)
    def get_messages(self, tid, before):
        log("fbchat: fetchThreadMessages({}, before={})", tid, before)
        fb_messages = self.client.fetchThreadMessages(thread_id=tid, before=before)
        messages = []
        participant_info = {}
        for fb_message in fb_messages:
            base = {
                "id": fb_message.uid,
                "timestamp": int(fb_message.timestamp),
                "sender": fb_message.author,
            }
            if fb_message.text:
                messages.append({"type": "text", "content": fb_message.text, **base})
            for attachment in fb_message.attachments:
                if isinstance(attachment, fbchat.FileAttachment):
                    messages.append({"type": "file", "content": attachment.url, **base})
                elif isinstance(attachment, fbchat.ImageAttachment):
                    log("fbchat: fetchImageUrl({})", attachment.uid)
                    messages.append(
                        {
                            "type": "image",
                            "content": self.client.fetchImageUrl(attachment.uid),
                            **base,
                        }
                    )
                else:
                    messages.append(
                        {
                            "type": "unsupported",
                            "unsupported": type(attachment).__name__,
                            **base,
                        }
                    )
            for p in fb_message.read_by:
                if p not in participant_info:
                    participant_info[p] = {"lastSeenMessage": fb_message.uid}
        return {"messages": messages, "participants": participant_info}

    @wrap_fbchat(require_login=True)
    def send_message(self, cid, mtype, content):
        # TODO: do I need to specify the thread_type in order to send
        # to a group?
        if mtype == "text":
            if len(content) > 80:
                display_content = repr(content[:80]) + "..."
            else:
                display_content = repr(content)
            log("fbchat: send({}, thread_id={})", display_content, cid)
            self.client.send(fbchat.Message(text=content), thread_id=cid)
        elif mtype == "image" or mtype == "file":
            log("fbchat: sendLocalFiles({}, thread_id={})", repr(content), cid)
            self.client.sendLocalFiles([content], thread_id=cid)
        else:
            raise ValueError("uh oh, unknown message type: {}".format(mtype))
