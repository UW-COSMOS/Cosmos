export DOCKER_BUILDKIT=1
if [ -z "$1" ]
then
    export VERSION=latest
else 
    export VERSION=$1
fi
echo "Creating images tagged with $VERSION"
docker build -t uwcosmos/cosmos-base:$VERSION -f deployment/cosmos.Dockerfile .
docker build --build-arg VERSION=latest -t uwcosmos/cosmos-ingestion:$VERSION -f deployment/ingestion.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-api:$VERSION -f deployment/api.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-retrieval:$VERSION -f deployment/retrieval.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-linking:$VERSION -f deployment/linking.Dockerfile .

#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-base-cpu:$VERSION -f deployment/cosmos-cpu.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-ingestion-cpu:$VERSION -f deployment/ingestion-cpu.Dockerfile .

#docker build -t iaross/cosmos-api:dev_doifix -f deployment/api.Dockerfile .
