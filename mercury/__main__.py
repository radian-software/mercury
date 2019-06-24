import argparse
import json
import sys

import mercury


def send_message(message):
    json.dump(message, sys.stdout)
    print()


def main():
    parser = argparse.ArgumentParser(
        description="Emacs interface to Facebook Messenger",
    )
    parser.add_argument(
        "--no-load-session", dest="load_session", action="store_false",
        help="Don't automatically log in from saved session cookies",
    )
    args = parser.parse_args()
    server = mercury.Server(send_message, load_session=args.load_session)
    while True:
        try:
            line = input()
        except EOFError:
            break
        if not line:
            continue
        try:
            message = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(message, dict):
            continue
        server.handle_message(message)
    sys.exit(0)


if __name__ == "__main__":
    main()
