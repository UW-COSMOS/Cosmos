#export DOCKER_BUILDKIT=1
if [ -z "$1" ]
then
    export VERSION=latest
else 
    export VERSION=$1
fi
echo "Creating images tagged with $VERSION"

#docker build -t uwcosmos/cosmos-base:$VERSION -f deployment/cosmos.Dockerfile .
#if [ $? -eq 0 ]; then
#    echo "Base image tagged with $VERSION"
#else
#    echo "Could not create base image! Exiting."
#    exit 1
#fi
docker build --build-arg VERSION=$VERSION -t uwcosmos/cosmos-ingestion:$VERSION -f deployment/ingestion.Dockerfile .
if [ $? -eq 0 ]; then
    echo "Ingestion image tagged with $VERSION"
else
    echo "Could not create ingestion image! Exiting."
    exit 1
fi
docker build --build-arg VERSION=$VERSION -t uwcosmos/cosmos-service:$VERSION -f deployment/cosmos-service.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-api:$VERSION -f deployment/api.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-retrieval:$VERSION -f deployment/retrieval.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-linking:$VERSION -f deployment/linking.Dockerfile .

#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-base-cpu:$VERSION -f deployment/cosmos-cpu.Dockerfile .
#docker build --build-arg VERSION=latest -t uwcosmos/cosmos-ingestion-cpu:$VERSION -f deployment/ingestion-cpu.Dockerfile .

#docker build -t iaross/cosmos-api:dev_doifix -f deployment/api.Dockerfile .
