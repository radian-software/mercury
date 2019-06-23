import argparse
import json
import sys

import mercury


def broadcast(message):
    json.dump(message, sys.stdout)


def main():
    parser = argparse.ArgumentParser(
        description="Emacs interface to Facebook Messenger",
    )
    parser.parse_args()
    server = mercury.Server(broadcast)
    while True:
        line = input()
        if not line:
            continue
        request = json.loads(line)
        response = server.send_request(request)
        json.dump(response, sys.stdout)
    sys.exit(0)


if __name__ == "__main__":
    main()
