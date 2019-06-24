import argparse
import json
import sys

import mercury


def main():
    parser = argparse.ArgumentParser(
        description="Emacs interface to Facebook Messenger",
    )
    parser.parse_args()
    server = mercury.Server(lambda message: json.dump(message, sys.stdout))
    while True:
        line = input()
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
