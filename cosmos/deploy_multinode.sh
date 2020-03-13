export MYSQL_USER=myuser
export MYSQL_DATABASE=cosmos
export MYSQL_PASSWORD=cosmos123
export REDIS_PTH=/ssd/ankur/redis_data
export UID=$(id -u)
export GID=$(id -g)
docker stack deploy cosmos --compose-file docker-compose-multinode.yml
docker service scale cosmos_ingestion=$1
docker service scale cosmos_process_pages=$1
docker service scale cosmos_detect=$1

