docker build -t process:latest -f process_pages/Dockerfile .
docker run -it -v $1:/vol process:latest bash
