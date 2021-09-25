!/bin/bash

docker build -f Dockerfile-node-config -t ssledz/cardano-alonzo-purple-node-config:latest .
docker push ssledz/cardano-alonzo-purple-node-config:latest
