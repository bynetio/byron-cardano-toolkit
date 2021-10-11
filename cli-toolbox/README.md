Simple usage
============

```
export PATH=~/git/byron-cardano-toolkit/cli-toolbox/:$PATH
```

```
query-utxo.sh $(wallet.sh -a my-default)
```

```
mint-ft.sh -w ssledz -d $(./wallet.sh -a ssledz) -u policy1 -t Beer3 -n 100 -v 10000000
```

```
pay-to-wallet.sh -w ssledz -d $(./wallet.sh -a ssledz2) -v 10000000 -t "100 15b54c6aa9cdc6bff221b7513567413485a2dbb127693dff994f674d.Beer3"
```

