from dataclasses import dataclass
from enum import Enum, unique
from typing import Optional


@unique
class SquareType(str, Enum):
    EMPTY = "_e"
    DOUBLE_LETTER = "DL"
    DOUBLE_WORD = "DW"
    OCCUPIED = "||"


@dataclass
class Square:
    type: SquareType
    priority: int = 0
    char: Optional[str] = None

    def __str__(self) -> str:
        if self.char is not None:
            return f"_{self.char}"
        else:
            return self.type.value

    def place_char(self, char: str) -> None:
        self.char = char
        self.type = SquareType.OCCUPIED

    @classmethod
    def empty(cls) -> "Square":
        return cls(SquareType.EMPTY)

    @classmethod
    def double_letter(cls) -> "Square":
        return cls(SquareType.DOUBLE_LETTER)

    @classmethod
    def double_word(cls) -> "Square":
        return cls(SquareType.DOUBLE_WORD)
