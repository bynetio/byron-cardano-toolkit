# Byron Cardano Toolkit
This repository is a set of codebases that help to operate with [Cardano Blockchain](https://cardano.org)

## [Cardano Wallet](cardano-wallet/)
### Init


```
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
```

### API

#### create wallet

```
curl -X PUT -d '{"cwpName":"my supper wallet" }' \ 
  -H 'Accept: application/json' \
  -H 'Content-type: application/json' http://localhost:8081/wallet | jq 
```

#### list all wallets

```
curl -s  http://localhost:8081/wallet | jq
```

```
[
  {
    "address": "addr_test1qq4nu8aq93u7lfvfqfktrfykfd07awnefwya2y65yzvh78vuyls3cs59dtuujk3dv44zsjnfd0m58pzmhgyar4rtwwzq676m54",
    "identifier": "94debeb2-f961-4630-bff2-73e9670095de",
    "name": "my supper wallet",
    "desc": null
  },
  {
    "address": "addr_test1qrfpcwqkx44e5zzngvrmnqhky6r6mhvz7k4snu7j525vcw59pr6lw6gk3xlgjz5tr45y9093yd4c72l0t048puqq6mzs50kaee",
    "identifier": "8bcbf5bb-c66b-44d3-8e27-beb1a6a34891",
    "name": "my supper wallet",
    "desc": null
  },
  {
    "address": "addr_test1qz6utx3eml6yeluh6lyv38dxnexufxegv5r02ysgwvxm494q8sp0p8qeqlk97mcxy3u9axgqgvn3rfpcyd8myqe4vtsqt37qkp",
    "identifier": "46777e5e-b035-4c4f-9d39-9bfd290101b1",
    "name": "my supper wallet",
    "desc": null
  },
  {
    "address": "addr_test1qq42k2xul69xmq2pc3lp3a64mdw3vmerhcp2lmalwgur4crecnfhzql9qxpxy4mgx3e392y69pyyqep0gcwwqmnqtvzsxv8u9q",
    "identifier": "800d8734-2621-4ba6-a6aa-192882002f58",
    "name": "ssledz",
    "desc": "descriptoin"
  }
]
```

#### get wallet by id

```
curl -s http://localhost:8081/wallet/800d8734-2621-4ba6-a6aa-192882002f58 | jq
```

```
{
  "address": "addr_test1qq42k2xul69xmq2pc3lp3a64mdw3vmerhcp2lmalwgur4crecnfhzql9qxpxy4mgx3e392y69pyyqep0gcwwqmnqtvzsxv8u9q",
  "identifier": "800d8734-2621-4ba6-a6aa-192882002f58",
  "name": "ssledz",
  "desc": "descriptoin"
}
```

#### sign transaction

```
cat tx.draft
```

```
{
  "cborHex": "86a50081825820c6a50a7ee73623e74ab14e2fd41bac22bd3212803cecef9cd8230d1f6cf5ebe8000d8001828258390071341d496bf6f2fa83461dc659b2722642d8c80a70ea49fab368ffef0fd7036ab6f53b98f93c95775548304a538ccb5d27e314a5f363d01b1b000000ba43ac781c82583900428f8a26d486a201d46594b8c40c2f2529399038c8f15e03665d60c9fc0b5c5f2ba41094c22c358f3d6a8beeedc884fe3cb572ac02e9711b1b0000000ba43b7400021a0002b1f90e809fff8080f5f6",
  "description": "",
  "type": "TxBodyAlonzo"
}
```

```
cat tx.draft \
  | curl -s -X POST -d @- -H 'Content-type: application/json' \
    http://localhost:8081/wallet/800d8734-2621-4ba6-a6aa-192882002f58/signTx \
  | jq
```

```
{
    "type": "Tx AlonzoEra",
    "description": "",
    "cborHex": "84a50081825820c6a50a7ee73623e74ab14e2fd41bac22bd3212803cecef9cd8230d1f6cf5ebe8000d8001828258390071341d496bf6f2fa83461dc659b2722642d8c80a70ea49fab368ffef0fd7036ab6f53b98f93c95775548304a538ccb5d27e314a5f363d01b1b000000ba43ac781c82583900428f8a26d486a201d46594b8c40c2f2529399038c8f15e03665d60c9fc0b5c5f2ba41094c22c358f3d6a8beeedc884fe3cb572ac02e9711b1b0000000ba43b7400021a0002b1f90e80a100818258201682aaf50ca7059442bef35149daa677cb9cc86e5ff0f1635ba917587701504a5840e556db1a4a967251e3d58f283464cc0e0f4a7b052750879b44208e9b61ce7c97aae00ee05ed360db9b2bebe0797fe7636309c390686a83d1246a25856b53c707f5f6"
}
```
