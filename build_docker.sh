#!/bin/bash
HOST=localhost
PORT=5001
IMAGE=dl_coxph

docker build -t $IMAGE -t $HOST:$PORT/$IMAGE .
docker push $HOST:$PORT/$IMAGE