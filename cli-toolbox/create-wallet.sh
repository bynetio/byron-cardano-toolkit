#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} [-h] wallet_name

  Create wallet consisting of following assets: 
    * wallet_name/payment.skey
    * wallet_name/payment.vkey
    * wallet_name/stake.skey
    * wallet_name/stake.vkey
    * wallet_name/wallet.addr

  where wallet_name is a directory passed as a parameter

  -h                          Print this help.

EOF
}

wallet_name=

while getopts "h" opt; do

  case $opt in
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

shift $((OPTIND-1)) # Shift off the options and optional --

wallet_name=$1

required=(wallet_name)

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

export TESTNET_MAGIC=7

[[ -d $wallet_name ]] && echo "Wallet $wallet_name already exists" && exit 1

mkdir -p $wallet_name

sandbox_dir=$(pwd)/$wallet_name

assert_cardano_node_exists

list payment.vkey payment.skey stake.vkey stake.skey wallet.addr \
    | peek lambda n . touch $sandbox_dir/'$n' > /dev/null

node_cli address key-gen \
	 --verification-key-file /out/payment.vkey \
	 --signing-key-file /out/payment.skey

node_cli stake-address key-gen \
	 --verification-key-file /out/stake.vkey \
	 --signing-key-file /out/stake.skey

node_cli  address build \
	  --payment-verification-key-file /out/payment.vkey \
	  --stake-verification-key-file /out/stake.vkey \
	  --out-file /out/wallet.addr \
	  --testnet-magic $TESTNET_MAGIC
