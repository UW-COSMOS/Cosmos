import click
from evaluate.evaluate import run_evaluate,calculate_statistics_map, make_pie_charts


@click.command()
@click.argument("preds_dir")
@click.argument("annotations_dir")
def run_eval(preds_dir, annotations_dir):
    fp_list = run_evaluate(preds_dir, annotations_dir)
    smap = calculate_statistics_map(fp_list)
    make_pie_charts(smap)


if __name__ == "__main__":
    run_eval()


