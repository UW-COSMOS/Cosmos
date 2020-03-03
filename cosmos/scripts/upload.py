import requests
import base64
import uuid


with open('test.pdf', 'rb') as rf:
    bstring = base64.b64encode(rf.read()).decode()
    result = requests.post('http://it:8000/preprocess', json={'pdf': bstring, 'dataset_id': str(uuid.uuid4())})
print(result)
print(result.json())



