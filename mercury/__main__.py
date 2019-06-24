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
    parser.parse_args()
    server = mercury.Server(send_message)
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
