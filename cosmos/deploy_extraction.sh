export DB_PTH=./db
export DB_TMP_PTH=./dbtmp
export TMP_PTH=./tmp
export DIR=/home/iaross/test
export DATASET_ID=dummy

if [ ! -d $DB_PTH ]
then
    mkdir $DB_PTH
fi
chmod 777 $DB_PTH

if [ ! -d $DB_TMP_PTH ]
then
    mkdir $DB_TMP_PTH
fi
chmod 777 $DB_TMP_PTH

if [ ! -d $TMP_PTH ]
then
    mkdir $TMP_PTH
fi
chmod 777 $TMP_PTH



export MYSQL_USER=myuser
export MYSQL_DATABASE=cosmos
export MYSQL_PASSWORD=cosmos123
export MYSQL_HOST=mysql-server-1
export MYSQL_PORT=3306
docker stack deploy cosmos --compose-file docker-compose-extraction.yml

# TODO: retrieval stuff?
