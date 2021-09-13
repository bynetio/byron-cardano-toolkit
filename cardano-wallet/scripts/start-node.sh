#!/bin/bash

docker volume create node-ipc
docker volume create config
docker volume create data

docker run \
    --rm \
    -v config:/config \
    ssledz/cardano-alonzo-purple-node-config:latest

docker run \
    -d \
    --name cardano-node \
    -v node-ipc:/opt/cardano/ipc \
    -v data:/opt/cardano/data \
    -v config:/opt/cardano/config \
    -p 3001:3001 \
    inputoutput/cardano-node:1.29.0-rc2 \
    run \
    --config /opt/cardano/config/alonzo-purple-config.json \
    --topology /opt/cardano/config/alonzo-purple-topology.json \
    --port 3001
