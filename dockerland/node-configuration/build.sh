#!/bin/bash

docker build -f alozno-purple/Dockerfile -t ssledz/cardano-node-config:alonzo-purple .
docker push ssledz/cardano-node-config:alonzo-purple

mkdir -p testnet/out
curl https://hydra.iohk.io/build/7370192/download/1/testnet-db-sync-config.json \
  | jq '.NodeConfigFile="node-config.json"' > testnet/out/testnet-db-sync-config.json
docker build -f testnet/Dockerfile -t ssledz/cardano-node-config:testnet .
docker push ssledz/cardano-node-config:testnet
