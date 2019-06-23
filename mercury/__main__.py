import argparse
import json
import sys

import mercury


def main():
    parser = argparse.ArgumentParser(description="Emacs interface to Facebook Messenger")
    args = parser.parse_args()
    command = json.load(sys.stdin)
    response = mercury.run_command(command)
    json.dump(response, sys.stdout, indent=2)
    print()
    sys.exit(0)


if __name__ == "__main__":
    main()
