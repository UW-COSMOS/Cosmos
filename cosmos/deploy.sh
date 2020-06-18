export MYSQL_ROOT_PASSWORD=
export MYSQL_USER=
export MYSQL_DATABASE=
export MYSQL_PASSWORD=
export MYSQL_HOST=
export MYSQL_PORT=
export DATABASE_DIR=
export UID=$(id -u)
export GID=$(id -g)
docker stack deploy cosmos --compose-file docker-compose-database.yml
