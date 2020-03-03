import requests
import json


url = 'http://process_pages:8001/process'
result = requests.post('http://pt:8004/process', json={'id': 50})
print(result)
print(result.json())
