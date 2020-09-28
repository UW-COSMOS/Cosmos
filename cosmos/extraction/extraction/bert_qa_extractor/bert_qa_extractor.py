from extraction.extractor import Extractor
from extraction.utils import cudafy, remove_padding, collate


class BertQAExtractor(Extractor):
    def __init__(self, qa_model, tokenizer):
        self.tokenizer = tokenizer
        self.qa_model = qa_model

    def extract(self, query, context):
        c = self.tokenizer.encode_plus(query, context, return_tensors="pt",
                                       max_length=512, truncation=True, padding=True)
        q_len = c['input_ids'].squeeze().numpy().tolist().index(102)
        c = remove_padding(c, self.tokenizer.pad_token_id)
        padding_value = self.tokenizer.pad_token_id
        c = collate([c], padding_value)
        c = cudafy(c)
        start_scores, end_scores = self.qa_model(**c)
        start_scores = start_scores[0][q_len+1:-1]
        end_scores = end_scores[0][q_len+1:-1]
        max_answer_length = 30
        results = []
        for sind, s in enumerate(start_scores):
            for eind, e in enumerate(end_scores):
                if eind <= sind:
                    continue
                if eind - sind > max_answer_length:
                    continue
                results.append((sind, eind, s + e))
        max_result = max(results, key=lambda x: x[2])
        start, end, score = max_result
        answer = c['input_ids'][0, q_len+1+start:q_len+2+end].detach().cpu().numpy().tolist()
        answer = self.tokenizer.decode(answer)
        return answer, score

