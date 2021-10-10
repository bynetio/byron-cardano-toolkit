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

list_wallets() {
  echo -e "Id\t\t\t\t\tName\t\tDesc\n=============================================================="; 
  find $WALLETS_DIR -iname meta.json | xargs cat | jq -r '([.identifier, .name, .desc]) | @tsv'
}

find_one_wallet() {
  local id=$1
  wallet=$(wallet_path_by_id $id)
    cnt=$(list $wallet | wc -l)
    if [[ $cnt -gt 1 ]]; then 
      (1>&2 echo -e "\nThere is more then one wallet identified by $id\n")
      (1>&2 list_wallets)
      exit 1
    elif [[ $cnt -eq 0 ]]; then
      (1>&2 echo "There is no such wallet identified by $id")
      exit 2
    else
      echo $wallet
    fi
}

print_wallet() {
  local id=$1
  local asset=$2
  wallet=$(find_one_wallet $id) # can't be local
  [[ $? -eq 0 ]] && echo $(cat $wallet/$asset)
}

case $cmd in
  "create")
    create_wallet $wallet_name "$wallet_desc"
    ;;
  "ls")
     list_wallets
    ;;
  "print_address")
    print_wallet $wallet_identifier "wallet.addr"
    ;;
  "print_key")
    wallet=$(find_one_wallet $wallet_identifier)
    [[ $? -eq 0 ]] && echo "$wallet/payment.skey"
    ;;
esac