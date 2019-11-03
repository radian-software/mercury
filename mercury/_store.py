"""
Read and write the files on disk that have session data and saved
messages from each account.
"""

import copy
import json
import operator
import string

import atomicwrites

from mercury import _util as util

# Name of file containing session data, i.e. cookies and such. This is
# saved so that you don't have to keep logging in every time you
# restart Mercury. This is stored in a separate file because you might
# want to back up your messages file but the cookies are sensitive
# information that should probably be stored somewhere else.
_SESSIONS_FILE = util.MERCURY_DIR / "sessions.json"

# Name of file containing all the messages we've downloaded ever. This
# is saved so that we can show you existing messages instantly,
# instead of having to fetch them from the server like a normal
# webapp.
_MESSAGES_FILE = util.MERCURY_DIR / "messages.json"

# Global variable containing the session data read from
# _SESSIONS_FILE. It's a dictionary mapping session IDs to arbitrary
# string dictionaries. See _read_sessions for the exact format.
_global_sessions = None


def _read_sessions():
    """
    Read the session data from disk, if not already cached. Throw an
    assert error if it's malformed. Don't return anything.
    """
    global _global_sessions
    if _global_sessions is not None:
        return
    try:
        with open(_SESSIONS_FILE) as f:
            _global_sessions = json.load(f)
    except FileNotFoundError:
        _global_sessions = {}
    assert isinstance(_global_sessions, dict)
    for key, val in _global_sessions.items():
        assert isinstance(key, str)
        assert isinstance(val, str)


def _write_sessions():
    """
    Write the current session data to disk. You need to call
    _read_sessions first.
    """
    global _global_sessions
    _SESSIONS_FILE.parent.mkdir(parents=True, exist_ok=True)
    with atomicwrites.atomic_write(_SESSIONS_FILE, overwrite=True) as f:
        json.dump(_global_sessions, f, indent=2)
        f.write("\n")


def get_session(aid):
    """
    Return the session data for the given account ID. If there's no
    session data for that account ID, return None.
    """
    global _global_sessions
    assert isinstance(aid, str)
    _read_sessions()
    return _global_sessions.get(aid)


def set_session(aid, session):
    """
    Make a copy of the given session data and set it for the given
    account ID.
    """
    global _global_sessions
    assert isinstance(aid, str)
    assert isinstance(aid, (str, type(None)))
    _read_sessions()
    if session is not None:
        _global_sessions[aid] = session
    else:
        _global_sessions.pop(aid)
    _write_sessions()


# The account data we read from disk, including all of the messages
# and such. It's a dictionary where the "accounts" key is a dictionary
# mapping account IDs to dictionaries with information about the users
# and conversations in the account. See _read_account_data for the
# exact format.
_global_account_data = None


def _assert_valid_account_data(account_data):
    """
    Throw an assert error if the data for the given account is
    malformed. The data is one value in the overall account data
    dictionary (under the "accounts" key).
    """
    assert isinstance(account_data, dict)
    assert isinstance(account_data["name"], str)
    assert isinstance(account_data["users"], dict)
    for uid, user in account_data["users"].items():
        assert isinstance(uid, str)
        assert isinstance(user, dict)
        assert isinstance(user["name"], str)
    assert isinstance(account_data["conversations"], list)
    for conversation in account_data["conversations"]:
        assert isinstance(conversation, dict)
        assert isinstance(conversation["id"], str)
        assert isinstance(conversation["name"], str)
        assert isinstance(conversation["timestamp"], int)
        assert isinstance(conversation["participants"], dict)
        for uid, participant in conversation["participants"].items():
            assert isinstance(uid, str)
            assert isinstance(participant, dict)
            assert isinstance(participant["lastSeenMessage"], (str, type(None)))
            assert uid in account_data["users"]
        assert isinstance(conversation["messages"], list)
        for message in conversation["messages"]:
            assert isinstance(message, dict)
            assert isinstance(message["id"], str)
            assert message["type"] in {"text", "image", "file"}
            assert isinstance(message["content"], str)
            assert isinstance(message["timestamp"], int)
            assert isinstance(message["sender"], str)
        assert len(set(m["id"] for m in conversation["messages"])) == len(
            conversation["messages"]
        )
        assert util.is_sorted(conversation["messages"], key=lambda m: -m["timestamp"])
    assert len(set(c["id"] for c in account_data["conversations"])) == len(
        account_data["conversations"]
    )
    # Unfortunately, because of Messenger, we can't guarantee that
    # conversations are sorted by timestamp. Rip.


def _read_account_data():
    """
    Read the account data from disk, if it's not already cached. Don't
    return anything. Throw an assert error if the data is malformed.
    """
    global _global_account_data
    if _global_account_data is not None:
        return
    try:
        with open(_MESSAGES_FILE) as f:
            _global_account_data = json.load(f)
    except FileNotFoundError:
        _global_account_data = {"version": 1, "accounts": {}}
    assert isinstance(_global_account_data, dict)
    assert isinstance(_global_account_data["version"], int)
    assert _global_account_data["version"] == 1
    assert isinstance(_global_account_data["accounts"], dict)
    for key, val in _global_account_data["accounts"].items():
        assert isinstance(key, str)
        _assert_valid_account_data(val)


def _write_account_data():
    """
    Write the current account data to disk. You need to call
    _read_account_data first.
    """
    global _global_account_data
    _MESSAGES_FILE.parent.mkdir(parents=True, exist_ok=True)
    with atomicwrites.atomic_write(_MESSAGES_FILE, overwrite=True) as f:
        json.dump(_global_account_data, f, indent=2)
        f.write("\n")


def get_account_data(aid):
    """
    Return a (deep) copy of all the account data for the given account
    ID.
    """
    global _global_account_data
    assert isinstance(aid, str)
    _read_account_data()
    return copy.deepcopy(_global_account_data["accounts"].get(aid))


def set_account_data(aid, account_data):
    """
    Update the account data for the given account ID to point at a
    (deep) copy of the provided data.
    """
    global _global_account_data
    assert isinstance(aid, str)
    _assert_valid_account_data(account_data)
    _read_account_data()
    _global_account_data["accounts"][aid] = copy.deepcopy(account_data)
    _write_account_data()
