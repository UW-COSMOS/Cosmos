"""
Generate a shapes dataset
Author: Josh McGrath
"""
import click

from scene.scene import Scene
from scene.config import Config


@click.command()
@click.argument("config")
@click.option("--n", help="the number of examples to generate", default=10)
@click.option("--out", help="output directory of pngs and annotation xmls", default="./out")
def main(config, n):
    print(config, n)


if __name__ == "__main__":
    main()