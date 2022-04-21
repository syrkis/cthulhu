from src.player import Player


class DefaultAgent(Player):
    name: str = "Default"

    def play(self):
        raise NotImplementedError
