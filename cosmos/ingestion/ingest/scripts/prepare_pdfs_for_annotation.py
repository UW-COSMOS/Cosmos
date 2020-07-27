from ingest.ingest import Ingest
import click

@click.command()
@click.option('--pdfs-path', type=str, help='Path to pdfs directory')
@click.option('--target-path', type=str, help='Path to directory where images will be saved.')
@click.option('--tmp-path', type=str, help='Path to a temporary directory')
def prepare(pdfs_path, target_path, tmp_path):
    ingest = Ingest('tcp://localhost:8786', tmp_dir=tmp_path)
    ingest.write_images_for_annotation(pdfs_path, target_path)

if __name__ == '__main__':
    prepare()
