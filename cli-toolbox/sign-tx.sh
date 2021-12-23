#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh


show_help() {
  cat << EOF

  Usage: ${0##*/} -w wallet tx_file [-h]

  Application description.
      
  -h                          Print this help.
  -w wallet

EOF
}

payment_key_path=

while getopts ":w:h" opt; do

  case $opt in 
    w)
      wallet=$OPTARG
      payment_key_path=$($dir/wallet.sh -k $wallet)
      [[ $? -ne 0 ]] && exit 1
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

tx_file=$1

required=(tx_file payment_key_path)

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

sandbox_dir=$(new_sandbox)

touch $sandbox_dir/tx.signed
cp $payment_key_path $sandbox_dir/payment.skey
cp $tx_file $sandbox_dir/tx.draft

sign_tx payment.skey

cat $sandbox_dir/tx.signed

rm -rf $sandbox_dir
