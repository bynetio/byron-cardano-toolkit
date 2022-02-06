#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} -w wallet -u policy-wallet -d dest_addr -t token-name -n token-amount [-v amount_of_lovelase] [-m token-meta-json-path] [-b slots] [-h]

  Application description.

  -w wallet                   Minting wallet (implicates source address and payment key)
  -u policy-wallet            Determines policy key pairs
  -d dest_addr                Destination address.
  -v lovelace_amount
  -t token-name
  -n token-amount
  -m token-meta-json-path
  -b slots-after-now          Number of slots since now after which we are not allowed to mint and burn
  -h                          Print this help.

EOF
}

payment_addr=
payment_key_path=
policy_vkey_path=
policy_skey_path=
lovelace_amount=3344798
dest_addr=
token_name=
token_amount=
token_meta_path=
slots_after_now=

while getopts ":w:d:m:v:n:t:u:b:h" opt; do

  case $opt in
    v)
      lovelace_amount=$OPTARG
      ;;
    b)
      slots_after_now=$OPTARG
      ;;
    d)
      dest_addr=$OPTARG
      ;;
    m)
      token_meta_path=$OPTARG
      ;;
    n)
      token_amount=$OPTARG
      ;;
    t)
      token_name=$OPTARG
      ;;
    w)
      wallet=$OPTARG
      payment_addr=$($dir/wallet.sh -a $wallet)
      [[ $? -ne 0 ]] && exit 1
      payment_key_path=$($dir/wallet.sh -k $wallet)
      ;;
    u)
      policy_wallet=$OPTARG
      policy_skey_path=$($dir/wallet.sh -k $policy_wallet)
      [[ $? -ne 0 ]] && exit 1
      policy_vkey_path=$($dir/wallet.sh -j $policy_wallet)
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

required=(payment_addr payment_key_path lovelace_amount dest_addr policy_vkey_path policy_skey_path token_name token_amount)

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

hash_from_key() {
  node_cli address key-hash \
    --payment-verification-key-file /out/$1
}

default_token_meta_json() {
  cat << EOF
{
   "721": {
     "{{POLICY_ID}}": {
        "{{TOKEN_NAME}}": {
            "name": "{{TOKEN_NAME}}"
       }
     }
   }
}
EOF
}

gen_multi_policy_script() {
    local key_hash=$1
    local slot=$(expr $(get_tip | jq .slot?) + $slots_after_now)
    cat << EOF
{
  "type": "all",
  "scripts":
  [
    {
      "type": "before",
      "slot": $slot
    },
    {
      "type": "sig",
      "keyHash": "$key_hash"
    }
  ]
}
EOF
}


gen_policy_script() {
    if [[ -z $slots_after_now ]]; then
        gen_simple_policy_script "$@"
    else
        gen_multi_policy_script "$@"
    fi
}

gen_simple_policy_script() {
    local key_hash=$1
    cat << EOF
    {
        "keyHash" : "$key_hash",
        "type"    : "sig"
    }
EOF
}

get_policy_id() {
  node_cli transaction policyid \
    --script-file /out/$1
}

token_meta_json_process() {
  local path=$1
  local policy_id=$2
  local token_name=$3
  cat $path \
    | sed -e "s/{{TOKEN_NAME}}/$token_name/g" -e "s/{{POLICY_ID}}/$policy_id/g"
}

init_sandbox() {
  sandbox_dir=$(new_sandbox)
  touch $sandbox_dir/tx.draft
  touch $sandbox_dir/tx.signed
  cp $payment_key_path $sandbox_dir/payment.skey
  cp $policy_skey_path $sandbox_dir/policy.skey
  cp $policy_vkey_path $sandbox_dir/policy.vkey
  get_protocol_params > $sandbox_dir/protocol.json
  gen_policy_script $(hash_from_key policy.vkey) > $sandbox_dir/policy.script
  get_policy_id policy.script > $sandbox_dir/policy.id
  if [[ -z $token_meta_path ]]; then 
    token_meta_path=$sandbox_dir/token_meta-template.json
    default_token_meta_json > $token_meta_path
  fi
  local policy_id="$(cat $sandbox_dir/policy.id)"
  token_meta_json_process $token_meta_path "$policy_id" "$token_name" > $sandbox_dir/token_meta.json
}

build_raw_tx() {

  utxo_in=$(get_utxo $payment_addr)

  utxo_in_value=$(get_utxo_value_at_tx $payment_addr $utxo_in)

  policy_id=$(cat $sandbox_dir/policy.id)

  token_value="$token_amount $policy_id.$token_name"

  node_cli transaction build-raw \
    --tx-in "$utxo_in" \
    --tx-out $dest_addr+$lovelace_amount+"$token_value" \
    --tx-out $payment_addr+0 \
    --mint "$token_value" \
    --minting-script-file /out/policy.script \
    --alonzo-era \
    --fee 0 \
    --out-file /out/tx.draft
#    --metadata-json-file /out/token_meta.json \

  node_cli transaction calculate-min-fee \
    --tx-body-file /out/tx.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 2 \
    --protocol-params-file /out/protocol.json $NETWORK > $sandbox_dir/fee.txt

  fee=$(cat $sandbox_dir/fee.txt | cut -d' ' -f1)

  result_balance=$(echo "$utxo_in_value-$fee-$lovelace_amount" | bc)

  node_cli transaction build-raw \
    --tx-in $utxo_in \
    --tx-out $dest_addr+$lovelace_amount+"$token_value" \
    --tx-out $payment_addr+$result_balance \
    --alonzo-era \
    --fee $fee \
    --mint="$token_value" \
    --minting-script-file /out/policy.script \
    --out-file /out/tx.draft
#    --metadata-json-file /out/token_meta.json \

#   node_cli transaction build \
#     --tx-in $utxo_in \
#     --tx-out $dest_addr+$lovelace_amount+"$token_value" \
#     --alonzo-era \
#     --mint="$token_value" \
#     --minting-script-file /out/policy.script \
#     --metadata-json-file /out/token_meta.json \
#     --change-address $payment_addr $NETWORK \
#     --out-file /out/tx.draft

  [[ $? -ne 0 ]] && exit 1

}

assert_cardano_node_exists

init_sandbox

build_raw_tx

sign_tx payment.skey policy.skey

cat <<EOF


      Payment address     : $payment_addr 
      Destination address : $payment_addr

      Send value          : $lovelace_amount+"$token_value"
      
EOF

[[ $(submit_tx | tail -1) == "true" ]] && loop_query_utxo $dest_addr

rm -rf $sandbox_dir

