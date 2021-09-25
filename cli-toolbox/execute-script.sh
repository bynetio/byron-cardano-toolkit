#!/bin/bash

source lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} -p path_to_script -w wallet_addres -d datum_path -c clollateral_addres [-r redeemer_path] -k path_to_pkey -e script_execution_units (-f fee | -g fee_coeff) [-s num_slot_exp]  [-h]

  Application description.

  -p path_to_script           Path to script.
  -w wallet_addres            Wallet address
  -d datum_path               Path to the file with datum.
  -r redeemer_path            Path to the file with redeemer.
  -k path_to_pkey             Path to wallet private key required to signe a transaction.
  -c clollateral_address      Collateral address to pay for script in case of failure.
  -s num_slot_exp             Number of slots after current slot to make transaction invalid
  -e script_execution_units   Script execution units in a form of tupple like "(200000000,200000000)"
  -f fee
  -g fee_coeff                Coefficent used to compute fee based on script execution units
  -h                          Print this help.

EOF
}

wallet_addr=
script_path=
datum_path=
redeemer_path=
pkey_path=
collateral_addr=
num_slot_exp=300
script_exe_units=
fee=
fee_coeff=

while getopts ":p:w:f:g:e:s:c:d:r:c:k:h" opt; do
    
    case $opt in
	e)
	    script_exe_units=$OPTARG
	    if [[ ! "$script_exe_units" =~ \([0-9]+,[0-9]+\) ]]; then
		echo >&2
		echo "  Invalid script execution unit option: $script_exe_units" >&2
		show_help
		exit 1
	    fi
	    ;;
	w)
	    wallet_addr=$OPTARG
	    ;;
	f)
	    fee=$OPTARG
	    ;;
	g)
	    fee_coeff=$OPTARG
	    ;;
	p)
	    script_path=$OPTARG
	    ;;
	d)
	    datum_path=$OPTARG
	    ;;
	r)
	    redeemer_path=$OPTARG
	    ;;    
	k)
	    pkey_path=$OPTARG
	    ;;
	c)
	    collateral_addr=$OPTARG
	    ;;
	s)
	    num_slot_exp=$OPTARG
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

[[ -z $fee ]] && [[ -z $fee_coeff ]] && echo && echo "  Please specify at least one of fee or fee_coeff" && show_help && exit 1

required=(script_path collateral_addr num_slot_exp script_exe_units datum_path wallet_addr pkey_path)

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

export TESTNET_MAGIC=7

#----- assert cardano node runs -----

assert_cardano_node_exists

#----- initializing sandbox -----

sandbox_dir=$(pwd)/.execute-script-$(date +"%Y%m%dT%H%M%S")

mkdir -p $sandbox_dir

cp $script_path $sandbox_dir

cp $pkey_path $sandbox_dir/wallet.skey

touch $sandbox_dir/tx.draft

touch $sandbox_dir/tx.signed

cat $datum_path > $sandbox_dir/datum.json

if [[ -z $redeemer_path ]]; then
    echo '{"int":0}' > $sandbox_dir/redeemer.json
else    
    cat $redeemer_path > $sandbox_dir/redeemer.json
fi

#----- grabing script address -----

script_addr=$(get_script_address $(basename $script_path))

#----- computing datum hash -----

datum_hash=$(get_datum_hash datum.json)

echo "Datum hash is: $datum_hash"

#----- selecting utxo at script address -----

echo -e "select utxo at script address: $script_addr"
read -n1 -p "filter utxo by datum? [y/n]: " ans
echo
utxo_in_script=$(get_utxo $script_addr "$([[ $ans == 'y' ]] && echo $datum_hash)")

echo "selected $utxo_in_script"

#----- selecting collateral utxo at wallet address -----

echo -e "select collateral utxo"

utxo_in_collateral=$(get_utxo $collateral_addr)

#----- computing script slot expiration -----

current_slot=$(get_current_slot)
tx_expiry_slot=$((current_slot+num_slot_exp))

#-----  get protocol parameters ----

get_protocol_params > $sandbox_dir/protocol.json

#----- computing fee -----

if [[ -z $fee ]]; then
    cpu_units=$(tupl $script_exe_units)
    mem_units=$(tupr $script_exe_units)
    fee=$(echo "($cpu_units+$mem_units)*$fee_coeff"|bc)
fi

#----- computing spendable from scrip (minus fee) -----

tx_ins="$utxo_in_script"

is_negative() {
  echo "$1<0" | bc    
}

utxo_in_script_value=$(get_utxo_value_at_tx $script_addr $utxo_in_script)
utxo_in_script_data=$(get_utxo_data_at_tx $script_addr $utxo_in_script)
aditional_value=0

compute_spendable() {
    echo "$utxo_in_script_value-$fee+$aditional_value"|bc
}

spendable=$(compute_spendable)

while [[ $(is_negative $spendable) -eq 1 ]]; do
    echo "Spendable $spendable is negative"
    read -n1 -p "Add aditionl utxo from wallet $wallet_addr [y/n] > " ans < /dev/tty
    echo
    if [[ $ans == 'y' ]]; then
	aditional_utxo_in=$(get_utxo $wallet_addr)
	aditional_value=$(echo "$aditional_value+$(get_utxo_value_at_tx $wallet_addr $aditional_utxo_in)"|bc)
	spendable=$(compute_spendable)
	tx_ins="$aditional_utxo_in --tx-in $tx_ins"
	echo "Spendable so far: $spendable"
	echo "Transaction inputs so far: $tx_ins"
    else
	echo 'Exiting'
	rm -rf $sandbox_dir
	exit 1
    fi
done

#---- building transaction draft -----

node_cli transaction build-raw $(echo --alonzo-era \
				      --invalid-hereafter $tx_expiry_slot \
				      --fee $fee \
				      --tx-in-collateral $utxo_in_collateral \
				      --tx-in $tx_ins \
				      --tx-in-script-file /out/$(basename $script_path) \
				      --tx-in-redeemer-file /out/redeemer.json \
				      --tx-in-datum-file /out/datum.json \
				      --tx-in-execution-units "$script_exe_units" \
				      --tx-out "$wallet_addr+$spendable" \
				      --protocol-params-file /out/protocol.json \
				      --out-file /out/tx.draft | xargs)

echo -e '\ntransaction draft:\n'
cat $sandbox_dir/tx.draft | jq
echo

node_cli transaction sign \
  --tx-body-file /out/tx.draft \
  --signing-key-file /out/wallet.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /out/tx.signed

echo -e '\nsigned transaction:\n'
cat $sandbox_dir/tx.signed | jq
echo

echo
cat <<EOF
   script path            : $script_path
   script addr            : $script_addr
   wallet addr            : $wallet_addr
   datum hash             : $datum_hash
   utxo script in         : $utxo_in_script
   utxo script in value   : $utxo_in_script_value
   utxo scritp data       : $utxo_in_script_data
   utxo in collateral     : $utxo_in_collateral
   current slot           : $current_slot
   exp     slot           : $tx_expiry_slot
   fee                    : $fee
   spendable from script  : $spendable
EOF
echo

read -n1 -p 'Submit transaction [y/n] > ' ans < /dev/tty
echo
if [[ $ans == 'y' ]]; then
  echo -e "\nSubmiting transaction...\n"
  node_cli transaction submit \
    --tx-file /out/tx.signed \
    --testnet-magic $TESTNET_MAGIC
  loop_query_utxo $wallet_addr
fi

rm -rf $sandbox_dir
