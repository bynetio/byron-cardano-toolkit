#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} [-h] [-l] [-c wallet_name [-d desc]] [-a wallet_name|id] [-k wallet_name|id]

  -c wallet_name         Creates wallet consisting of the following assets: 
                           * payment.skey
                           * payment.vkey
                           * stake.skey
                           * stake.vkey
                           * wallet.addr
                           * meta.json

  -d desc                Wallet description

  -a wallet_name|id      Print payment address of a given wallet_name (wallet id)

  -k wallet_name|id      Return path to secure payment key

  -l                     list all wallets

  -h                     Print this help.

  All wallets can be found here: $WALLETS_DIR

EOF
}

cmd=
wallet_name=
wallet_desc=
wallet_identifier=

while getopts ":a:k:d:c:lh" opt; do

  case $opt in
    c)
      wallet_name="$OPTARG"
      cmd="create"
      ;;
    d)
      wallet_desc="$OPTARG"
      ;;
    l)
      cmd="ls"
      ;;  
    a)
      cmd="print_address"
      wallet_identifier="$OPTARG"
      ;;
    k)
      cmd="print_key"
      wallet_identifier="$OPTARG"
      ;;      
    h)
      show_help
      exit 0
      ;;
    \?)
      echo >&2
      echo "  Invalid option: -$OPTARG" >&2
      show_help
      exit 1
      ;;
    :)
      echo >&2
      echo "  Option -$OPTARG requires an argument" >&2
      show_help
      exit 2
      ;;
    *)
      show_help
      exit 3
      ;;
  esac

done

shift $((OPTIND-1))

[[ -z $cmd ]] && show_help && exit 1

create_wallet() {

  wallet_name="$1"
  wallet_desc="$2"

  wallet_identifier=$(gen_uuid)

  wallet_dir=$WALLETS_DIR/$wallet_identifier

  mkdir -p $wallet_dir

  sandbox_dir=$wallet_dir

  assert_cardano_node_exists

  list payment.vkey payment.skey stake.vkey stake.skey wallet.addr \
      | peek lambda n . touch $sandbox_dir/'$n' > /dev/null

  node_cli address key-gen \
	   --verification-key-file /out/payment.vkey \
	   --signing-key-file /out/payment.skey

  node_cli stake-address key-gen \
	   --verification-key-file /out/stake.vkey \
	   --signing-key-file /out/stake.skey

  node_cli address build \
	    --payment-verification-key-file /out/payment.vkey \
	    --stake-verification-key-file /out/stake.vkey \
	    --out-file /out/wallet.addr $NETWORK \

  echo "Wallet $wallet_identifier ($wallet_name) created"
  
  meta() {
    cat << EOF
  {
    "address":"$(cat $wallet_dir/wallet.addr)",
    "identifier":"$wallet_identifier",
    "name":"$wallet_name",
    "desc":"$wallet_desc"
  }
EOF
  }

  meta | jq > $wallet_dir/meta.json

}

wallet_path_by_id() {
  local id=$1
  find $WALLETS_DIR -iname meta.json | while read line; do
      if [[ $(cat $line | jq -r '.name, .identifier' | egrep "^$id$" | wc -l) -eq 1 ]]; then
        dirname $line
      fi
    done
}

case $cmd in
  "create")
    create_wallet $wallet_name "$wallet_desc"
    ;;
  "ls")
     echo -e "Id\t\t\t\t\tName\t\tDesc\n=============================================================="; 
     find $WALLETS_DIR -iname meta.json | xargs cat | jq -r '([.identifier, .name, .desc]) | @tsv'
    ;;
  "print_address")
    wallet_path_by_id $wallet_identifier | while read line; do
      echo $(cat $line/wallet.addr)
    done
    ;;
  "print_key")
    wallet_path_by_id $wallet_identifier | while read line; do
      echo $line/payment.skey
    done
    ;;
esac