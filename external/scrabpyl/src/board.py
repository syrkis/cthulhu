from copy import deepcopy
from enum import Enum

from src.dictionary import DICTIONARY
from src.exceptions import InvalidMoveError
from src.square import Square

# Abbreviations to make board creation more readable
_e = Square.empty
DL = Square.double_letter
DW = Square.double_word


class WordDirectionEnum(str, Enum):
    RIGHT = "R"
    DOWN = "D"


class Board:
    cells: list[list]

    def __init__(self, cells: list[list]):
        self.cells = cells

    def __str__(self) -> str:
        return "\n".join([" ".join(list(map(str, line))) for line in self.cells])

    def place_tiles(self, tiles: dict[tuple[int, int], str]) -> None:
        """
        Attempts to place tiles on the board.
        tiles is a dictionary of (x, y) coordinates to characters.
        """
        old_cells = deepcopy(self.cells)
        for (x, y), char in tiles.items():
            self.cells[y][x].place_char(char)

        if not self.is_valid():
            self.cells = old_cells
            raise InvalidMoveError("Invalid board")

    def _valid_word_placement(
        self, x: int, y: int, direction: WordDirectionEnum
    ) -> bool:

        if direction == WordDirectionEnum.RIGHT:
            check_word = x - 1 < 0 or self.cells[y][x - 1].char is None
        else:
            check_word = y - 1 < 0 or self.cells[y - 1][x].char is None

        if check_word:
            chars = []

            while (
                x < len(self.cells[y])
                and y < len(self.cells)
                and self.cells[y][x].char is not None
            ):
                chars += self.cells[y][x].char
                if direction == WordDirectionEnum.RIGHT:
                    x += 1
                else:
                    y += 1

            word = "".join(chars)
            if len(word) > 1 and word not in DICTIONARY:
                return False

        return True

    def is_valid(self) -> bool:
        for y, row in enumerate(self.cells):
            for x, _ in enumerate(row):
                if not self._valid_word_placement(x, y, WordDirectionEnum.RIGHT):
                    return False
                if not self._valid_word_placement(x, y, WordDirectionEnum.DOWN):
                    return False
        return True

    @classmethod
    def make_standard_board(cls) -> "Board":
        return cls(
            # fmt: off
            [
                [DW(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DW()],  # NOQA : E501

                [_e(), DL(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DL(), _e()],  # NOQA : E501

                [_e(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), _e(), _e()],  # NOQA : E501

                [_e(), _e(), DL(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DL(), _e(), _e()],  # NOQA : E501

                [_e(), DL(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DL(), _e()],  # NOQA : E501

                [DW(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), _e(), DW()],  # NOQA : E501
            ]
            # fmt: on
        )
