#!/bin/bash

docker build -f alozno-purple/Dockerfile -t ssledz/cardano-node-config:alonzo-purple .
docker push ssledz/cardano-node-config:alonzo-purple

docker build -f testnet/Dockerfile -t ssledz/cardano-node-config:testnet .
docker push ssledz/cardano-node-config:testnet