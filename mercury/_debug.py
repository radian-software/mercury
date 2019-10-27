import json

from mercury import _messenger as messenger


def get_service():
    service = messenger.MessengerService()
    try:
        with open("/home/raxod502/.fbchat") as f:
            cookies = json.load(f)["cookies"]
    except (FileNotFoundError, json.JSONDecodeError):
        pass
    else:
        service.restore_session(cookies)
    return service
