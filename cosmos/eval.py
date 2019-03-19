import click
from evaluate.evaluate_iccv import evaluate_dir
from evaluate.evaluate_libs import run_voc, run_coco
import os
import yaml
with open("classes.yaml") as stream:
  classes = yaml.load(stream)["classes"]

@click.command()
@click.argument("preds_dir")
@click.argument("annotations_dir")
@click.argument("out_d")
def run_eval(preds_dir, annotations_dir, out_d):
    iccv,confusion = evaluate_dir(preds_dir, annotations_dir, classes)
    voc = run_voc(preds_dir, annotations_dir,classes)
    os.mkdir(out_d)
    iccv.to_csv(os.path.join(out_d, "iccv.csv"))
    voc.to_csv(os.path.join(out_d,"voc.csv"))
    confusion.to_csv(os.path.join(out_d, "confusion.csv"))


if __name__ == "__main__":
    run_eval()


