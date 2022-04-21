from pathlib import Path


class Dictionary(set):
    @classmethod
    def from_txt(cls, path: Path) -> "Dictionary":
        with path.open("r") as f:
            return cls(f.read().splitlines())


DICTIONARY = Dictionary.from_txt(
    Path("../../ScrabbleTemplate/Dictionaries/English.txt")
)
