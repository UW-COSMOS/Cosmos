from connected_components.connected_components import run_write_proposals
import click


@click.command()
@click.argument("img_dir")
@click.argument("out_dir")
@click.argument("procs")
def cli(img_dir, out_dir, procs):
    run_write_proposals(img_dir=img_dir, output_dir=out_dir, procs=int(procs))


if __name__ == "__main__":
    cli()
