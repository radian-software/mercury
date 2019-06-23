import fbchat


class Server:

    def run_command(self, command):
        return {
            "echo": command,
        }
