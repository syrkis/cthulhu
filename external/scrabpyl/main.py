from src.agents import DefaultAgent, ManualAgent
from src.game import game_loop

if __name__ == "__main__":
    game_loop(
        player_constructors=[
            DefaultAgent,
            ManualAgent,
        ],
        print_delay=0.25,
    )
