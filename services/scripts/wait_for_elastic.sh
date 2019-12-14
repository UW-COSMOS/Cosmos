#!/bin/sh
# wait_for_elastic.sh

set -e
cmd="$@"

until curl --write-out %{http_code} --output /dev/null "es01:9200/_cluster/health?wait_for_status=green"; do
    sleep 1
done

>&2 echo "Elastic search online. Starting ingestion."
exec $cmd

