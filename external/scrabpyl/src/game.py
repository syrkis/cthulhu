import os
from time import sleep
from typing import Callable

from src.board import Board
from src.exceptions import InvalidMoveError
from src.player import Player


def game_loop(
    player_constructors: list[Callable[[Board], Player]], print_delay: float = 0.0
):
    board = Board.make_standard_board()
    players = [player_constructor(board) for player_constructor in player_constructors]

    for round in range(5):
        for player_id, player in enumerate(players):

            os.system("cls||clear")
            print(f"ROUND: {round} - {player} ({player_id})'s turn")
            print(board)

            try:
                player.play()
            except InvalidMoveError as e:
                raise e
                print(f"{player} made an invalid move")

            sleep(print_delay)
