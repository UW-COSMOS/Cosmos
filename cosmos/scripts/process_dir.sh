# python read_dir_and_request.py /vol
docker run --network cosmos_swarm_network -v $1:/vol uwcosmos/request:dev python read_dir_and_request.py /vol $2 &> err.txt
