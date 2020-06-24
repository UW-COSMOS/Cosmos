export DB_PTH=./db
export DB_TMP_PTH=./dbtmp
export TMP_PTH=./tmp
export MYSQL_USER=myuser
export MYSQL_DATABASE=cosmos
export MYSQL_PASSWORD=cosmos123
export MYSQL_HOST=mysql-server-1
export MYSQL_PORT=3306
docker stack deploy cosmos --compose-file docker-compose.yml
docker service scale cosmos_ingestion=$1
docker service scale cosmos_scheduler=$1
docker service scale cosmos_worker1=$1
docker service scale cosmos_worker2=$1
docker service scale cosmos_adminer=$1
docker service scale cosmos_detect=$1
docker service scale cosmos_mysql-server-1=$1


