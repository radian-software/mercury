"""
Internal API that each Mercury backend implements. Not to be confused
with the actual web APIs that we are scripting against (made by
Facebook etc.) and also not to be confused with the message-passing
API that the Mercury server uses to communicate with a client over
stdio. We define this API so that writing a backend is as simple as
possible (anything that doesn't have to be backend-specific is handled
at a higher level, such as message persistence and error correction).
"""

import abc


class LoginRequiredError(Exception):
    """
    Exception to throw when we can't complete the operation because we
    need some login credentials.
    """

    pass


class ServiceError(Exception):
    """
    Exception to throw when we get an upstream API error unexpectedly,
    or the API returns bad data.
    """

    def __init__(self, fmt, *args, **kwargs):
        super().__init__(fmt.format(*args, **kwargs))


class Service(abc.ABC):
    @abc.abstractmethod
    def __init__(self):
        """
        Construct a new instance of the service. It shouldn't need to know
        its own account ID, as that stuff is dealt with at a higher
        level. This shouldn't be doing anything fancy.
        """
        pass

    @abc.abstractmethod
    def get_session(self):
        """
        Return information that can be passed to restore_session, a
        string. If there's no active session, return None.
        """
        pass

    @abc.abstractmethod
    def restore_session(self, session):
        """
        Try to restore the session from the given account session data, a
        string. Throw LoginRequiredError if the auth fails.
        """
        pass

    @abc.abstractmethod
    def get_login_fields(self):
        """
        Return a list of fields that are required to do a login from
        scratch. The return value must always be the same.
        """
        pass

    @abc.abstractmethod
    def login(self, fields):
        """
        Given values for the fields returned by get_login_fields (a map
        where the keys are the "field" values from get_login_fields
        and the values are the strings provided by the user), try to
        do a login. Throw LoginRequiredError if the auth fails,
        ServiceError on unexpected error.
        """
        pass

    @abc.abstractmethod
    def logout(self):
        """
        Terminate the existing session, if any. Throw ServiceError if
        something goes wrong.
        """
        pass

    @abc.abstractmethod
    def get_you(self):
        """
        Return the user ID that the service uses to represent you. If this
        isn't how the upstream API works, this can be something fake
        that will work with the implementation of get_users.
        """
        pass

    @abc.abstractmethod
    def get_users(self, uids):
        """
        Given a list of user IDs, return a map from those user IDs to
        dictionaries with a "name" key.
        """
        pass

    @abc.abstractmethod
    def get_conversations(self, before):
        """
        Return data about conversations, and possibly also users,
        messages, and read receipts.
        """
        pass

    @abc.abstractmethod
    def get_messages(self, tid, before):
        """
        Return data about messages, and possibly also users and read
        receipts.
        """
        pass

    @abc.abstractmethod
    def send_message(self, cid, mtype, content):
        """
        Send a message. This is synchronous; if the send fails, throw
        ServiceError.
        """
