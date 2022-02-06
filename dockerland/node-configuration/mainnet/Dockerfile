FROM tianon/true


ADD https://hydra.iohk.io/build/7370192/download/1/mainnet-config.json          /config/node-config.json
ADD https://hydra.iohk.io/build/7370192/download/1/mainnet-byron-genesis.json   /config/mainnet-byron-genesis.json
ADD https://hydra.iohk.io/build/7370192/download/1/mainnet-shelley-genesis.json /config/mainnet-shelley-genesis.json
ADD https://hydra.iohk.io/build/7370192/download/1/mainnet-alonzo-genesis.json  /config/mainnet-alonzo-genesis.json
ADD https://hydra.iohk.io/build/7370192/download/1/mainnet-topology.json        /config/topology.json
#ADD https://hydra.iohk.io/build/7370192/download/1/mainnet-db-sync-config.json  /config/db-sync-config.json
ADD mainnet/out/mainnet-db-sync-config.json                                             /config/db-sync-config.json
