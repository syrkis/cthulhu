from abc import ABC, abstractmethod
from typing import Optional
from uuid import uuid4

from src.board import Board


class Player(ABC):
    board: Board
    name: str = str(uuid4())

    def __init__(self, board: Board, name: Optional[str] = None):
        self.board = board
        if name is not None:
            self.name = name

    @abstractmethod
    def play(self) -> None:
        raise NotImplementedError

    def __str__(self) -> str:
        return self.name
