#!/usr/bin/env -S uv run --quiet

# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "typer-slim",
# ]
# ///

import typer
from typing_extensions import Annotated


def main(name: Annotated[str, typer.Argument()] = "world"):
    print(f"Hello, {name}!")


if __name__ == "__main__":
    typer.run(main)
