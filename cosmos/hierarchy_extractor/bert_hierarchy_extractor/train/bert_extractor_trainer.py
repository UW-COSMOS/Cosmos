from bert_hierarchy_extractor.datasets.train_dataset import TrainHierarchyExtractionDataset
from bert_hierarchy_extractor.utils import cudafy
from bert_hierarchy_extractor.logging.utils import log_metrics
from torch.utils.data import DataLoader
from transformers import AdamW, get_linear_schedule_with_warmup
import torch
import time
from tqdm import tqdm
from comet_ml import Experiment


class RerankingTrainer:
    def __init__(
        self,
        experiment: Experiment,
        model,
        data_path: str,
        base_model: str,
        bsz: int,
        num_workers: int,
        lr: float,
        weight_decay: float,
        warmup_updates: int,
        max_updates: int,
        accumulation_steps: int,
        validate_interval: int,
        save_metric: str,
        save_min: bool,
        device: str,
        seed=1,
        num_correct=lambda x, y: return 0,
    ):
        """
        :param model: Initialized model
        :param dataset_path: Path to dataset
        :param base_model: Path to base model
        :param bsz: Batch size
        :param num_workers: Num workers available
        :param lr: Learning rate
        :param weight_decay: weight decay
        :param warmup_updates: number of samples to warmup learning rate
        :param max_updates: max number of samples
        :param accumulation_steps: Number of batches to accumulate loss over before running an update
        :param validate_interval: num updates before validating
        :param save_metric: metric to use to save best model
        :param save_min: Whether we're looking to minimize or maximize the save metric
        :param seed: Random seed for iteration
        """

        torch.manual_seed(seed)
        self.experiment = experiment
        self.device = device
        self.model = model.to(device)
        self.max_accumulation = accumulation_steps
        print("Loading training dataset")
        self.train_dataset = TrainHierarchyExtractionDataset(data_path)
        #print("Loading validation dataset")
        #self.val_dataset = MSMarcoDataset(data_path, base_model, "val")
        self.train_dataloader = DataLoader(
            self.train_dataset,
            batch_size=bsz,
            num_workers=num_workers,
            pin_memory=True,
            shuffle=True,
            collate_fn=MSMarcoDataset.collate,
        )
        #self.val_dataloader = DataLoader(
        #    self.train_dataset,
        #    batch_size=bsz,
        #    num_workers=num_workers,
        #    pin_memory=True,
        #    shuffle=True,
        #    collate_fn=MSMarcoDataset.collate,
        #)
        self.bsz = bsz
        self.optimizer = AdamW(model.parameters(), lr=lr, weight_decay=0.01)
        self.scheduler = get_linear_schedule_with_warmup(
            self.optimizer,
            num_warmup_steps=warmup_updates,
            num_training_steps=max_updates,
        )
        self.max_updates = max_updates
        self.validate_interval = validate_interval

    def validate(self, validate_cap=None, best_save_metric=None):
        self.model.eval()
        val_cap = validate_cap if validate_cap is not None else len(self.val_dataloader)
        with tqdm(total=val_cap) as pbar:
            total_loss = 0
            total_correct = 0
            for ind, batch in enumerate(self.val_dataloader):
                if ind > val_cap:
                    break
                xs, labels = cudafy(batch)
                loss, logits = self.model(**xs, labels=labels)
                nc = num_correct(logits, labels)
                total_correct += nc
                total_loss += loss.detach().cpu().numpy()
                pbar.update(1)
            loss_per_sample = total_loss / val_cap / self.bsz
            accuracy = total_correct / val_cap / self.bsz
        metrics = {}
        metrics["val_loss"] = loss_per_sample
        metrics["val_accuracy"] = accuracy
        metrics["val_per_sample_loss"] = total_loss
        if best_save_metric is not None:
            if metrics[best_save_metric] <= self.current_best_metric:
                model.save_pretrained('best')
                self.current_best_metric = metrics[best_save_metric]
        return metrics

    def train(self):
        """
        """
        start_time = time.time()
        # Verify forward pass using validation loop
        metrics = self.validate(validate_cap=5)

        self.model.train()
        with tqdm(total=self.max_updates, desc='Number of updates') as pbar:
            total_updates = 1
            val_updates = 1
            while total_updates < self.max_updates:
                accumulation_steps = 0
                accumulation_loss = None
                for batch in self.train_dataloader:
                    xs, labels = cudafy(batch)
                    loss, _ = self.model(**xs, labels=labels)
                    if accumulation_loss is None:
                        accumulation_steps += 1
                        accumulation_loss = loss
                    elif accumulation_steps > self.max_accumulation:
                        self.optimizer.zero_grad()
                        accumulation_loss.backward()
                        torch.nn.utils.clip_grad_norm_(self.model.parameters(), 1.0)
                        self.optimizer.step()
                        self.scheduler.step()
                        pbar.update(1)
                        total_updates += 1
                        accumulation_steps = 0
                        accumulation_loss = loss
                        l = loss.detach().cpu().numpy()
                        metrics = {}
                        metrics["train_update_loss"] = l
                        metrics["train_per_sample_loss"] = l / self.bsz
                        # TODO: Accuracy, f1, etc metrics
                        log_metrics(self.experiment, metrics, total_updates)
                        if total_updates % self.validate_interval == 0:
                            metrics = self.validate(validate_cap=1000, best_save_metric=self.save_metric)
                            val_updates += 1
                            log_metrics(self.experiment, metrics, val_updates)

                    else:
                        accumulation_steps += 1
                        accumulation_loss += loss


        metrics = self.validate(validate_cap=1000)
        print(f"Final validation metrics: {metrics}")
        torch.save(self.model.state_dict(), 'last.pt')
        val_updates += 1
        log_metrics(self.experiment, metrics, val_updates)
        end_time = time.time()
        total_time = end_time - start_time
        print(f"Total train time: {total_time}")
