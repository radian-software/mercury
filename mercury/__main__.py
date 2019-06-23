import argparse
import json
import sys

import mercury


def main():
    parser = argparse.ArgumentParser(description="Emacs interface to Facebook Messenger")
    args = parser.parse_args()
    server = mercury.Server()
    while True:
        line = input()
        command = json.loads(line)
        response = server.run_command(command)
        json.dump(response, sys.stdout)
    sys.exit(0)


if __name__ == "__main__":
    main()
