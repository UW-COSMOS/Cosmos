from comet_ml import Experiment
import click
from transformers import BertForTokenClassification
from bert_hierarchy_extractor.train.bert_extractor_trainer import BertExtractorTrainer
from bert_hierarchy_extractor.model import BertHierarchyExtractor

@click.command()
@click.option("--data-path", type=str, help="Path to data")
@click.option("--bsz", type=int, help="Batch size")
@click.option("--num-workers", type=int, help="Number of workers")
@click.option("--lr", type=float, help="Learning rate")
@click.option("--weight-decay", type=float, help="Weight decay")
@click.option(
    "--warmup-updates", type=int, help="number of updates to warmup learning rate"
)
@click.option(
    "--max-updates", type=int, help="Maximum number of updates to iterate through"
)
@click.option(
    "--accumulation-steps", type=int, help="Number of accumulation steps to accumulate loss"
)
@click.option(
    "--validation-interval",
    type=int,
    help="Number of updates before running validation loop",
)
@click.option("--seed", type=int, help="Random seed for training")
@click.option("--model-path", type=str, help="Model to use")
@click.option("--tag", type=str, help="Experiment tag")
@click.option("--device", type=str, help="Device list to run on")
@click.option("--save-metric", type=str, help="Metric to save best model using")
@click.option("--save-min/--save-max", type=bool, default=False, help="Whether to save the minimum or maximum (default is maximum)")
def train_hierarchy_extractor(
                    data_path,
                    bsz,
                    num_workers,
                    lr,
                    weight_decay,
                    warmup_updates,
                    max_updates,
                    accumulation_steps,
                    validation_interval,
                    seed,
                    model_path,
                    tag,
                    device,
                    save_metric,
                    save_min,
                ):
    experiment = Experiment(project_name="information-retrieval", auto_output_logging=False)
    experiment.add_tags([tag])
    parameters = {
        "bsz": bsz,
        "num_workers": num_workers,
        "lr": lr,
        "weight_decay": weight_decay,
        "warmup_updates": warmup_updates,
        "max_updates": max_updates,
        "validation_interval": validation_interval,
        "seed": seed,
        "model_path": model_path,
        "device": device,
        "accumulation_steps": accumulation_steps,
        "save_metric": save_metric,
        "save_min": save_min
    }
    experiment.log_parameters(parameters)
    model = BertHierarchyExtractor(model_path, 12, device)
    trainer = BertExtractorTrainer(
        experiment,
        model,
        data_path,
        model_path,
        bsz,
        num_workers,
        lr,
        weight_decay,
        warmup_updates,
        max_updates,
        accumulation_steps,
        validation_interval,
        save_metric,
        save_min,
        device,
        seed,
    )
    try:
        trainer.train()
    except Exception as e:
        raise e


if __name__ == "__main__":
    train_hierarchy_extractor()
