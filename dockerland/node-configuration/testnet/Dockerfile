FROM tianon/true


ADD https://hydra.iohk.io/build/7370192/download/1/testnet-config.json          /config/node-config.json
ADD https://hydra.iohk.io/build/7370192/download/1/testnet-byron-genesis.json   /config/testnet-byron-genesis.json
ADD https://hydra.iohk.io/build/7370192/download/1/testnet-shelley-genesis.json /config/testnet-shelley-genesis.json
ADD https://hydra.iohk.io/build/7370192/download/1/testnet-alonzo-genesis.json  /config/testnet-alonzo-genesis.json
ADD https://hydra.iohk.io/build/7370192/download/1/testnet-topology.json        /config/topology.json
#ADD https://hydra.iohk.io/build/7370192/download/1/testnet-db-sync-config.json  /config/db-sync-config.json
ADD testnet/out/testnet-db-sync-config.json                                             /config/db-sync-config.json
