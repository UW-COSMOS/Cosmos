import click
from ingest.ingest import Ingest

@click.command()
@click.option('--pdfs-path', type=str, help='Path to pdfs directory')
@click.option('--target-path', type=str, help='Path to directory where pdfs will be saved.')
def remove(pdfs_path, target_path):
    Ingest.remove_watermarks(pdfs_path, target_path)


if __name__ == '__main__':
    remove()