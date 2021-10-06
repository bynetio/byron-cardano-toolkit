#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} -p path_to_script -s payment_address -l payment_value -d datum_path -k path_to_pkey [-h]

  Application description.

  -p path_to_script           Path to script.
  -s payment_address          Payment address.
  -l payemnt_value            Number of lovelaces to pay to the script.
  -d datum_path               Path to the file with datum.
  -k path_to_pkey             Path to private key.
  -h                          Print this help.

EOF
}


script_path=
payment_addr=
payment_value=
datum_path=
pkey_path=

while getopts ":p:s:k:l:d:h" opt; do

  case $opt in
    p)
      script_path=$OPTARG
      ;;
    d)
      datum_path=$OPTARG
      ;;
    s)
      payment_addr=$OPTARG
      ;;
    l)
      payment_value=$OPTARG
      ;;
    k)
      pkey_path=$OPTARG
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

required=(script_path payment_addr payment_value datum_path pkey_path)

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

#----- assert cardano node runs -----

assert_cardano_node_exists

#----- sandbox init -----

sandbox_dir=$(pwd)/.submit-script-$(date +"%Y%m%dT%H%M%S")

mkdir -p $sandbox_dir

cp $script_path $sandbox_dir
cp $pkey_path $sandbox_dir/payment.skey

script_addr=$(get_script_addr $(basename $script_path))

node_cli query protocol-parameters --testnet-magic $TESTNET_MAGIC > $sandbox_dir/protocol.json

cat $datum_path > $sandbox_dir/datum.json

#----- hash datum -----

datum_hash=$(get_datum_hash datum.json)

touch $sandbox_dir/tx.draft
touch $sandbox_dir/tx.signed

utxo_in=$(get_utxo $payment_addr)

utxo_in_value=$(get_utxo_value_at_tx $payment_addr $utxo_in)

node_cli transaction build-raw \
  --tx-in $utxo_in \
  --tx-out $script_addr+0 \
  --tx-out-datum-hash $datum_hash \
  --tx-out $payment_addr+0 \
  --alonzo-era \
  --fee 0 \
  --out-file /out/tx.draft

node_cli transaction calculate-min-fee \
  --tx-body-file /out/tx.draft \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic $TESTNET_MAGIC \
  --protocol-params-file /out/protocol.json > $sandbox_dir/fee.txt

fee=$(cat $sandbox_dir/fee.txt | cut -d' ' -f1)

utxo_out_value=$(echo "$utxo_in_value-$fee-$payment_value"|bc)

node_cli transaction build-raw \
  --tx-in $utxo_in \
  --tx-out $script_addr+$payment_value \
  --tx-out-datum-hash $datum_hash \
  --tx-out $payment_addr+$utxo_out_value \
  --alonzo-era \
  --fee $fee \
  --out-file /out/tx.draft

echo -e '\n\n-------------Transaction draft---------------'
cat $sandbox_dir/tx.draft | jq
echo -e '---------------------------------------------\n\n'

node_cli transaction sign \
  --tx-body-file /out/tx.draft \
  --signing-key-file /out/payment.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /out/tx.signed

echo -e '\n\n-------------Signed transaction---------------'
cat $sandbox_dir/tx.signed | jq
echo -e '---------------------------------------------\n\n'

echo
cat <<EOF
   script path     : $script_path
   script addr     : $script_addr
   datum           : $(cat $sandbox_dir/datum.json)
   datum hash      : $datum_hash
   payment addr    : $payment_addr
   utxo in         : $utxo_in
   utxo in value   : $utxo_in_value
   utxo out value  : $utxo_out_value
   fee             : $fee
EOF
echo

read -n1 -p 'Submit transaction [y/n] > ' ans < /dev/tty
echo
if [[ $ans == 'y' ]]; then
  echo -e "\nSubmiting transaction..."
  node_cli transaction submit \
    --tx-file /out/tx.signed \
    --testnet-magic $TESTNET_MAGIC
  loop_query_utxo $script_addr
fi

rm -rf $sandbox_dir
