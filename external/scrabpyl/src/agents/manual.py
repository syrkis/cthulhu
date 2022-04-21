from src.board import WordDirectionEnum
from src.player import Player


class ManualAgent(Player):
    name: str = "Manual"

    def play(self):
        """
        Manually inputs of the form:

        x,y,R|D,word

        Where:
            - x is the x coordinate of the first letter of the word
            - y is the y coordinate of the first letter of the word
            - R|D is the direction of the word (R for right, D for down)
            - word is the word to be placed
        """

        move = input("Type a move (x,y,R|D,word): ")
        x, y, direction, word = move.split(",")

        x: int = int(x)
        y: int = int(y)
        direction: WordDirectionEnum = WordDirectionEnum(direction)
        chars: list[str] = list(word.upper())

        move: dict = {}

        for char in chars:
            move[(x, y)] = char
            if direction == WordDirectionEnum.RIGHT:
                x += 1
            else:
                y += 1

        self.board.place_tiles(move)
