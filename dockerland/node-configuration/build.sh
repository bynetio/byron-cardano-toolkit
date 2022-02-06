#!/bin/bash

#docker build -f alozno-purple/Dockerfile -t ssledz/cardano-node-config:alonzo-purple .
#docker push ssledz/cardano-node-config:alonzo-purple

mkdir -p testnet/out
mkdir -p mainnet/out
curl https://hydra.iohk.io/build/7370192/download/1/testnet-db-sync-config.json \
  | jq '.NodeConfigFile="node-config.json"' > testnet/out/testnet-db-sync-config.json
curl https://hydra.iohk.io/build/7370192/download/1/mainnet-db-sync-config.json \
  | jq '.NodeConfigFile="node-config.json"' > mainnet/out/mainnet-db-sync-config.json
docker build -f testnet/Dockerfile -t ssledz/cardano-node-config:testnet .
docker build -f mainnet/Dockerfile -t ssledz/cardano-node-config:mainnet .
docker push ssledz/cardano-node-config:testnet
docker push ssledz/cardano-node-config:mainnet
