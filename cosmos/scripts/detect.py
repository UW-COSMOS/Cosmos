import pickle
import requests

with open('test_detect.pkl', 'rb') as rf:
    obj = pickle.load(rf)

result = requests.post('http://pt:8004/detect', json=obj)
print(result.json())

