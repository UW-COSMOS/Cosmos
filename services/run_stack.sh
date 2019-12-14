export MYSQL_ROOT_PASSWORD=secret123
export MYSQL_DATABASE=cosmos
export MYSQL_USER=cosmos
export MYSQL_PASSWORD=constellation
docker stack deploy -c docker-compose-database.yml cosmos
