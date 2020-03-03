docker run --network cosmos_swarm_network -e BSZ=$2 -v $1:/vol request:latest &> err.txt
