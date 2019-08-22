# Word embeddings

Expose a simple API that returns 10 words related to the `word` parameter.

## Usage

Download the model files, then build + run via the docker-compose file:


```
./get_data.sh
docker-compose up --build

# test it
curl -X GET localhost:5002/word2vec?word=whale
```
