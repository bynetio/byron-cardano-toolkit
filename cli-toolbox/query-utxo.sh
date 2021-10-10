#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} [-h] [-w] address|wallet

  Application description.

  -w                          Instead of address you can pass wallet name or wallet identifier
  -h                          Print this help.

EOF
}

address=
wallet=0

while getopts "wh" opt; do

  case $opt in
    w)
      wallet=1
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

shift $((OPTIND-1)) # Shift off the options and optional --

address=$1

required=(address)

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

assert_cardano_node_exists

if [[ $wallet -eq 1 ]]; then
  name=$address
  address=$($dir/wallet.sh -a $address)
  [[ $? -ne 0 ]] && exit 1
fi

loop_query_utxo $address
