from mercury._api import LoginRequiredError, ServiceError
from mercury._messenger import MessengerService
from mercury import _store as store
from mercury import _util as util


class ClientError(Exception):
    pass


class Server:
    def __init__(self, send_msg):
        self.send_msg = send_msg
        self.service = MessengerService()
        self.logged_in = False
        session = store.get_session("messenger")
        if session is not None:
            try:
                self.service.restore_session(session)
                self.logged_in = True
            except (LoginRequiredError, ServiceError):
                # We don't have a message to respond to, so just let
                # it go. Not a big deal.
                pass

    def _get_aid(self, data):
        aid = data.get("aid")
        if not isinstance(aid, str):
            raise ClientError("account ID missing or not a string")
        if aid != "messenger":
            raise ClientError("no account with ID: {}".format(aid))
        return aid

    def _handle_message(self, mtype, data):
        if mtype == "addAccount":
            raise ClientError("addAccount not yet implemented")
        if mtype == "removeAccount":
            raise ClientError("removeAccount not yet implemented")
        if mtype == "getAccounts":
            return {
                "messenger": {
                    "service": "messenger",
                    "name": "Messenger",
                    "loginRequired": not self.logged_in,
                    "loginFields": self.service.get_login_fields(),
                }
            }
        if mtype == "login":
            self._get_aid(data)
            try:
                self.service.logout()
            except (ClientError, LoginRequiredError):
                pass
            fields = data.get("fields")
            if not isinstance(fields, dict):
                raise ClientError("login fields missing or not a map")
            for key, value in fields.items():
                if not (isinstance(key, str) and isinstance(value, str)):
                    raise ClientError("login fields include non-strings")
            if set(fields) != set(f["field"] for f in self.service.get_login_fields()):
                raise ClientError("login fields do not match required field names")
            self.service.login(fields)
            self.logged_in = True
            store.set_session("messenger", self.service.get_session())
            return {}
        if mtype == "logout":
            self._get_aid()
            self.service.logout()
            self.logged_in = False
            store.set_session("messenger", None)
            return {}
        if mtype == "getConversations":
            self._get_aid(data)
            limit = data.get("limit")
            if not isinstance(limit, (int, type(None))):
                raise ClientError("limit is not an integer")
            if limit is not None and limit <= 0:
                return {"conversations": []}
            offset = data.get("offset", 0)
            if not isinstance(offset, int):
                raise ClientError("offset is not an integer")
            if offset < 0:
                limit += offset
                offset = 0
            account_data = store.get_account_data("messenger")
            if account_data is None:
                account_data = {"name": "Messenger", "users": {}, "conversations": []}
            existing_account_cids = {c["id"] for c in account_data["conversations"]}
            you = self.service.get_you()
            users_with_data_needed = set()
            users_with_data_fetched = set()
            service_data = self.service.get_conversations(before=None)
            if account_data["conversations"] and not service_data["conversations"]:
                raise ServiceError("upstream forgot about all our conversations")
            elif service_data["conversations"]:
                for conversation in service_data["conversations"]:
                    conversation["new"] = True
                while True:
                    if account_data["conversations"] and min(
                        c["timestamp"] for c in service_data["conversations"]
                    ) >= max(c["timestamp"] for c in account_data["conversations"]):
                        fetching_new_conversations = False
                        before = min(
                            c["timestamp"] for c in service_data["conversations"]
                        )
                    elif (
                        len(
                            set(
                                c["id"]
                                for c in service_data["conversations"]
                                + account_data["conversations"]
                            )
                        )
                        < (limit or 0) + offset
                    ):
                        fetching_new_conversations = False
                        before = min(
                            c["timestamp"]
                            for c in service_data["conversations"]
                            + account_data["conversations"]
                        )
                    else:
                        break
                    older_service_data = self.service.get_conversations(before=before)
                    if not older_service_data["conversations"]:
                        break
                    existing_service_cids = {
                        c["id"] for c in service_data["conversations"]
                    }
                    for conversation in older_service_data["conversations"]:
                        if conversation["id"] not in existing_service_cids:
                            conversation["new"] = fetching_new_conversations
                            service_data["conversations"].append(conversation)
                    for uid, user in older_service_data.get("users", {}).items():
                        if "users" not in service_data:
                            service_data["users"] = {}
                        if uid not in service_data["users"]:
                            service_data["users"][uid] = {}
                        name = user.get("name")
                        if name:
                            service_data["users"][uid]["name"] = name
                            users_with_data_fetched.add(uid)
            if len(set(c["id"] for c in service_data["conversations"])) != len(
                service_data["conversations"]
            ):
                raise ServiceError("upstream returned non-unique conversation IDs")
            prepend_conversations = []
            append_conversations = []
            cids_to_remove = set()
            for conversation in service_data["conversations"]:
                assert not conversation.get(
                    "messages"
                ), "can't handle eager message fetch yet"
                cid = conversation["id"]
                if cid in existing_account_cids:
                    existing_conversation = next(
                        c for c in account_data["conversations"] if c["id"] == cid
                    )
                    existing_conversation["name"] = conversation["name"]
                    existing_conversation["timestamp"] = conversation["timestamp"]
                    for uid in list(existing_conversation["participants"]):
                        if uid not in conversation["participants"]:
                            existing_conversation["participants"].pop(uid)
                        if uid not in existing_conversation["participants"]:
                            existing_conversation["participants"][uid] = {}
                        user = conversation["participants"][uid]
                        last_seen_message = user.get("lastSeenMessage")
                        if last_seen_message:
                            existing_conversation["participants"][uid][
                                "lastSeenMessage"
                            ] = last_seen_message
                    existing_conversation["participants"] = {
                        uid: {
                            "lastSeenMessage": participant.get("lastSeenMessage")
                            or existing_conversation["participants"].get(uid, {})[
                                "lastSeenMessage"
                            ]
                        }
                        for uid, participant in conversation.get(
                            "participants", {}
                        ).items()
                    }
                    (
                        prepend_conversations
                        if conversation["new"]
                        else append_conversations
                    ).append(existing_conversation)
                    cids_to_remove.add(cid)
                else:
                    (
                        prepend_conversations
                        if conversation["new"]
                        else append_conversations
                    ).append(
                        {
                            "id": conversation["id"],
                            "name": conversation["name"],
                            "timestamp": conversation["timestamp"],
                            "participants": {
                                uid: {
                                    "lastSeenMessage": participant.get(
                                        "lastSeenMessage"
                                    )
                                }
                                for uid, participant in conversation[
                                    "participants"
                                ].items()
                            },
                            "messages": [],
                        }
                    )
                users_with_data_needed.update(conversation["participants"])
            account_data["conversations"] = (
                prepend_conversations
                + [
                    c
                    for c in account_data["conversations"]
                    if c["id"] not in cids_to_remove
                ]
                + append_conversations
            )
            extra_user_info = self.service.get_users(
                users_with_data_needed - users_with_data_fetched
            )
            for uid, user in extra_user_info.items():
                if "users" not in service_data:
                    service_data["users"] = {}
                service_data["users"][uid] = {"name": user["name"]}
            for uid, user in service_data["users"].items():
                if "users" not in account_data:
                    account_data["users"] = {}
                account_data["users"][uid] = {"name": user["name"]}
            result = {
                "conversations": [
                    {
                        "id": c["id"],
                        "name": c["name"],
                        "timestamp": c["timestamp"],
                        "participants": sorted(
                            (
                                {
                                    "id": uid,
                                    "name": account_data["users"][uid]["name"],
                                    "you": uid == you,
                                }
                                for uid, p in c["participants"].items()
                            ),
                            key=lambda p: p["name"],
                        ),
                    }
                    for c in account_data["conversations"]
                ][offset : offset + limit]
            }
            store.set_account_data("messenger", account_data)
            return result
        if mtype == "getMessages":
            raise ClientError("getMessages not yet implemented")
        if mtype == "sendMessage":
            raise ClientError("sendMessage not yet implemented")
        raise ClientError("unknown message type: {}".format(mtype))

    def handle_message(self, client_msg):
        try:
            if not isinstance(client_msg, dict):
                raise ClientError("message not a map")
            mid = client_msg.get("id")
            mtype = client_msg.get("type")
            data = client_msg.get("data")
            if not isinstance(mid, str):
                raise ClientError("message ID missing or not a string")
            if not isinstance(mtype, str):
                raise ClientError("message type missing or not a string")
            if not isinstance(data, dict):
                raise ClientError("message data missing or not a map")
            data = self._handle_message(mtype, data)
            self.send_msg({"type": "response", "id": mid, "error": None, "data": data})
        except ClientError as e:
            self.send_msg(
                {"type": "response", "id": mid, "error": "client error: {}".format(e)}
            )
        except ServiceError as e:
            self.send_msg(
                {
                    "type": "response",
                    "id": mid,
                    "error": "unexpected error: {}".format(e),
                }
            )
        except LoginRequiredError:
            self._ask_for_login()
            self.send_msg({"type": "response", "id": mid, "error": "login required"})
