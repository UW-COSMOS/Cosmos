IMAGE=$1
VERSION=$2

GIT_HASH=$(git rev-parse HEAD)

export DOCKER_BUILDKIT=1
if [ -z "$VERSION" ]
then
    export VERSION=$GIT_HASH
else 
    export VERSION=$VERSION
fi

case $IMAGE in
    cosmos-base ) DOCKERFILE=cosmos ;;
    cosmos-ingestion ) DOCKERFILE=ingestion ;;
    cosmos-service ) DOCKERFILE=cosmos-service ;;
    cosmos-api ) DOCKERFILE=api ;;
    * ) echo "$IMAGE is not a supported cosmos image."
        exit 1 ;;
esac


echo "Creating image $IMAGE tagged with $VERSION"
docker build -t uwcosmos/cosmos-base:$VERSION -f deployment/cosmos.Dockerfile .
docker build --build-arg VERSION=$VERSION --build-arg GIT_HASH=$GIT_HASH -t uwcosmos/$IMAGE:$VERSION -f deployment/$DOCKERFILE.Dockerfile .
