#!/bin/bash

docker volume create data
docker volume create node-ipc
docker volume create config

docker run \
    --rm \
    -v config:/config \
    ssledz/cardano-alonzo-purple-node-config:latest
