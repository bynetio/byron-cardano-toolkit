shorten_addr() {
    local addr=$1
    cat <(echo $addr | splitc | take 20 | join) \
	<(echo ...) \
	<(echo $addr | splitc | drop 89 | join) \
	| join
}

ifn() {
    local pred=$1
    shift
    local f="$@"
    [[ ! $(eval "$pred") ]] && eval "$f"
}

ift() {
    local pred=$1
    shift
    local f="$@"
    [[ $(eval "$pred") ]] && eval "$f"
}

if_no_volume() {
    local name=$1
    shift
    ifn "docker volume ls -q -f name=$name" "$@"
}

if_no_container() {
    local name=$1
    shift
    ifn "docker ps -q -f name=$name" "$@"
}

if_container_stopped() {
    local name=$1
    shift
    ift "docker ps -q -f name=$name -f status=exited" "$@"
}

node_run() {
    list data node-ipc config | map λ vn . 'if_no_volume $vn docker volume create $vn'
    if_no_container cardano-node docker run \
		    --rm \
		    -v config:/config \
		    $CONFIG_IMAGE
    
    if_no_container cardano-node  docker run \
		    -d \
		    --name cardano-node \
		    -v node-ipc:/opt/cardano/ipc \
		    -v data:/opt/cardano/data \
		    -v config:/opt/cardano/config \
		    -p 3001:3001 \
		    $NODE_IMAGE \
		    run \
		    --config /opt/cardano/config/alonzo-purple-config.json \
		    --topology /opt/cardano/config/alonzo-purple-topology.json \
		    --port 3001
}

node_rm() {
    docker stop cardano-node
    docker rm cardano-node
    list data node-ipc config | map λ vn . 'docker volume rm $vn'
}

assert_cardano_node_exists() {

    ask_for_continuation() {
	echo -e "\nPlease wait couple of seconds, in order to give some time to node to fully synchronize with the ledger"
	echo -ne '\nContinue [y/n]: '
	read -n1 key < /dev/tty
	echo -e "\n\nSync progress: $(get_node_sync_progress)"
	echo
	if [[ $key == 'n' ]]; then
	    echo
	    exit 1
	fi
    }
    
    start_node_help() {

	cat <<EOF

Cardano node is stopped, please start it again using following command:

    docker start cardano-node

EOF

	local key
	read -p "I can start it for you [y/n]: " -n1 key < /dev/tty
	echo
	if [[ $key == 'y' ]]; then
	    docker start cardano-node > /dev/null
	    ask_for_continuation
	else
	    echo
	    exit 2
        fi
    }
    
    run_node_help() {
	
      
	cat <<EOF
Cardano node does not exists, run one using following commands:

    docker volume create data
    docker volume create node-ipc
    docker volume create config

    docker run \\
      --rm \\
      -v config:/config \\
      $CONFIG_IMAGE

    docker run \\
      -d \\
      --name cardano-node \\
      -v node-ipc:/opt/cardano/ipc \\
      -v data:/opt/cardano/data \\
      -v config:/opt/cardano/config \\
      -p 3001:3001 \\
      $NODE_IMAGE \\
      run \\
      --config /opt/cardano/config/alonzo-purple-config.json \\
      --topology /opt/cardano/config/alonzo-purple-topology.json \\
      --port 3001

EOF

	local key
	read -p "I can run the commnads for you [y/n]: " -n1 key < /dev/tty
	echo
	if [[ $key == 'y' ]]; then
	    node_run
	    ask_for_continuation
	else
	    echo
	    exit 4
        fi
    } 

  if_container_stopped cardano-node start_node_help  > /dev/tty
    
  if_no_container cardano-node run_node_help > /dev/tty
}

node_cli() {
    docker run \
	   --name node-cli \
	   --rm \
	   --entrypoint cardano-cli \
	   -e NETWORK=testnet \
	   -e CARDANO_NODE_SOCKET_PATH=/ipc/socket \
	   -v node-ipc:/ipc \
	   -v ${sandbox_dir}:/out \
	   $NODE_IMAGE $@    
}

get_tip() {
  node_cli query tip $NETWORK
}

get_node_sync_progress() {
  get_tip | jq -r '.syncProgress'    
}

get_utxos() {
    local addr=$1
    node_cli query utxo \
	     --address $addr \
	     --out-file /dev/stdout $NETWORK \
	| jq -c ". as \$obj | keys_unsorted | map(. as \$key | \$obj[\$key] | . + {tx: \$key})"
}

filter_utxo_by() {
    local filter=$1  
    cat - | jq -c " map(select($filter))"
}

filter_utxo_by_datum_hash() {
    local hash=$1  
    filter_utxo_by ".data==\"$hash\""
}

jhead() {
  jq -r '.[0]'    
}

filter_utxo_by_tx() {
    local tx=$1
    filter_utxo_by ".tx==\"$tx\"" | jhead
}


get_utxo() {
    local addr=$1
    local datum_hash=$2

    get_utxos_filtered() {
	[[ -z $datum_hash ]] && get_utxos $addr || get_utxos $addr | filter_utxo_by_datum_hash $datum_hash
    }
    
    local utxos=$(get_utxos_filtered | jq -r '.[].tx')
    local cnt=$(list $utxos | wc -l)
    if [[ $cnt -eq 1 ]]; then
	echo $utxos
    else
	echo "At a given address $(shorten_addr $addr) there are multiple utxos, choose one: $(seq 1 $cnt | join ,)" > /dev/tty
	get_utxos_filtered | jq > /dev/tty
	read  -p '> ' n < /dev/tty
	if [[ $(seq 1 $cnt | grep -e "^$n\$" | wc -l) -eq 0 ]]; then
	    echo 'No such element, choose again...' > /dev/tty
	    get_utxo $addr
	else
	    list $utxos | drop $((n-1)) | head -1
	fi
    fi
}

get_utxo_value_at_tx() {
    local addr=$1
    local tx=$2
    get_utxos $addr | filter_utxo_by_tx $tx | jq -r ".value.lovelace"
}

get_utxo_data_at_tx() {
    local addr=$1
    local tx=$2
    get_utxos $addr | filter_utxo_by_tx $tx | jq -r ".data"
}

qloop() {
    local message_fun=$1
    shift
    local cmds="$@"
    while [[ $key != 'q' ]]; do
	message=$(eval $message_fun)
	echo -e "\n$message ('q' for quit, enter for query again)\n"
	eval "$@"
	read -n1 key < /dev/tty
    done
    echo
}

sqloop() {
    local message=$1
    shift
    sqloop_message() {
	echo $message
    }
    qloop 'sqloop_message' "$@"
}

loop_query_utxo() {
  local addr=$1
  local key=''
  qloop_message() {
      echo "\nQuering utxo at $(shorten_addr $addr) (node sync: $(get_node_sync_progress))"
  }
  qloop 'qloop_message' \
	node_cli query utxo --address $addr $NETWORK
}

get_script_address() {
  local file_name=$1
  node_cli address build --payment-script-file /out/$file_name $NETWORK
}

get_current_slot() {
  node_cli query tip $NETWORK | jq '.slot'
}

get_protocol_params() {
  node_cli query protocol-parameters $NETWORK
}

get_datum_hash() {
  local file_name=$1  
  node_cli transaction hash-script-data --script-data-file /out/$file_name
}
