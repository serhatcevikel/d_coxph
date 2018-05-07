#!/bin/bash

# HOST=localhost
# PORT=5000

HOST=docker-registry.distributedlearning.online
PORT=443

IMAGE=dl_coxph

if [ $PORT -eq 443 ]
then
    docker build -t $IMAGE -t $HOST/$IMAGE .
    docker push $HOST/$IMAGE
else
    docker build -t $IMAGE -t $HOST:$PORT/$IMAGE .
    docker push $HOST:$PORT/$IMAGE
fi


