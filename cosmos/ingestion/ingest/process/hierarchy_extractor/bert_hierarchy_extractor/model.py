import torch
import torch.nn.functional as F
from transformers import BertForTokenClassification


def log_sum_exp(vec):
    max_score = vec[0, torch.argmax(vec, dim=2)]
    max_score_broadcast = max_score.view(1, -1).expand(1, vec.shape[1])
    return max_score + torch.log(torch.sum(torch.exp(vec - max_score_broadcast)))

class BertHierarchyExtractor(torch.nn.Module):
    # The CRF implementation is adapted from:
    # https://pytorch.org/tutorials/beginner/nlp/advanced_tutorial.html
    START_TAG = 1
    END_TAG = 2
    def __init__(self, model_path, num_labels, label_map, device):
        super().__init__()
        self.model = BertForTokenClassification.from_pretrained(model_path, num_labels=num_labels).to(device)
        self.transitions = torch.nn.Parameter(torch.randn(num_labels, num_labels))
        # Ok, so we're going to add some constraints here
        self.label_map = label_map
        self.num_labels = num_labels

    def _forward_algorithm(self, features):
        init_alphas = torch.full((1, self.num_labels), -10000)
        init_alphas[0][self.label_map[self.START_TAG]] = 0.
        forward_var = init_alphas

        for feat in features:
            alphas_t = []
            for next_tag in range(self.num_labels):
                emit_score = feat[next_tag].view(1, -1).expand(1, self.num_labels)
                trans_score = self.transitions[next_tag].view(1, -1)
                next_tag_var = forward_var + trans_score + emit_score
                alphas_t.append(log_sum_exp(next_tag_var).view(1))
            forward_var = torch.cat(alphas_t).view(1, -1)
        terminal_var = forward_var + self.transitions[self.label_map[self.STOP_TAG]]
        alpha = log_sum_exp(terminal_var)
        return alpha

    def _viterbi_decode(self, feats):
        backpointers = []
        init_vvars = torch.full((1, self.num_labels), -10000.)
        init_vvars[0][self.label_map[self.START_TAG]] = 0
        forward_var = init_vvars
        for feat in feats:
            bptrs_t = []
            viterbivars_t = []
            for next_tag in range(self.num_labels):
                next_tag_var = forward_var + self.transitions[next_tag]
                best_tag_id = torch.argmax(next_tag_var)
                bptrs_t.append(best_tag_id)
                viterbivars_t.append(next_tag_var[0][best_tag_id].view(1))
            forward_var = (torch.cat(viterbivars_t) + feat).view(1, -1)
            backpointers.append(bptrs_t)

        terminal_var = forward_var + self.transitions[self.label_map[self.STOP_TAG]]
        best_tag_id = torch.argmax(terminal_var)
        path_score = terminal_var[0][best_tag_id]
        best_path = [best_tag_id]
        for bptrs_t in reversed(backpointers):
            best_tag_id = bptrs_t[best_tag_id]
            best_path.append(best_tag_id)
        _ = best_path.pop()
        best_path.reverse()
        return path_score, best_path

    def _score_sequence(self, features, tags):
        score = torch.zeros(1).to(self.device)
        start = torch.LongTensor([self.label_map[self.START_TAG]]).to(self.device)
        tags = torch.cat([start, tags])
        for i, feat in enumerate(features):
            score += self.transitions[tags[i+1], tags[i]] + feat[tags[i+1]]
        score += self.transitions[self.label_map[self.STOP_TAG], tags[-1]]
        return score

    def forward(self, batch, labels=None, weights=None):
        bert_score = self.model(**batch)[0]

        if labels is None:
            return torch.transpose(bert_score, 1, 2)
        bert_score = torch.transpose(bert_score, 1, 2)
        loss = F.cross_entropy(bert_score, labels, ignore_index=-1, weight=weights)
        return loss, bert_score

    def save_pretrained(self, path):
        self.model.save_pretrained(path)
