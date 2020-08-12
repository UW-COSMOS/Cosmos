export SCHEDULER_ADDRESS=tcp://localhost:8786
export ELASTIC_ADDRESS=localhost
gunicorn -w 4 -b 127.0.0.1:4000 'cosmos:create_app()'