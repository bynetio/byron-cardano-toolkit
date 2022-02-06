shorten_addr() {
    local addr=$1
    cat <(echo $addr | splitc | take 20 | join) \
	<(echo ...) \
	<(echo $addr | splitc | drop 89 | join) \
	| join
}

trim() {
  sed -e 's/^[ ]*\(.*[^ ]\) *$/\1/g'
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

if_container_not_running() {
    local name=$1
    shift
    ifn "docker ps -q -f name=$name -f status=running" "$@"
}

if_container_running() {
    local name=$1
    shift
    ift "docker ps -q -f name=$name -f status=running" "$@"
}


if_container_stopped() {
    local name=$1
    shift
    ift "docker ps -q -f name=$name -f status=exited" "$@"
}

wait_for_psql_ready() {
    while true; do
        sleep 1
        echo 'waiting for psql to be ready'
        docker exec -it \
               ${STACK_PREFIX}_cardano-postgres \
               psql -U $POSTGRES_USER -c 'select 1' > /dev/null
        [[ $? -eq 0 ]] && break
    done
}

db_sync_run() {
    list ${STACK_PREFIX}_cardano-postgres \
         ${STACK_PREFIX}_db-sync-tmp \
         ${STACK_PREFIX}_db-sync-data \
        | map λ vn . 'if_no_volume $vn docker volume create $vn'

    if_no_container \
        ${STACK_PREFIX}_cardano-postgres docker run \
        --name ${STACK_PREFIX}_cardano-postgres \
        --env POSTGRES_LOGGING=true \
        --env POSTGRES_PASSWORD=$POSTGRES_PASSWORD \
        --env POSTGRES_DB=$POSTGRES_DB \
        --env POSTGRES_USER=$POSTGRES_USER \
        -v ${STACK_PREFIX}_cardano-postgres:/var/lib/postgresql/data \
        -p $POSTGRES_PORT:5432 \
        -d \
        $POSTGRES_IMAGE

    wait_for_psql_ready

    if_container_running \
        ${STACK_PREFIX}_cardano-node \
        if_no_container \
        ${STACK_PREFIX}_cardano-db-sync docker run \
        -d \
        --name ${STACK_PREFIX}_cardano-db-sync \
        --network host \
        --env POSTGRES_DB=$POSTGRES_DB \
        --env POSTGRES_PASSWORD=$POSTGRES_PASSWORD \
        --env POSTGRES_USER=$POSTGRES_USER \
        --env POSTGRES_HOST=127.0.0.1 \
        --env POSTGRES_PORT=$POSTGRES_PORT \
        --env RESTORE_SNAPSHOT=${RESTORE_SNAPSHOT:-} \
        --env RESTORE_RECREATE_DB=N \
        -v ${STACK_PREFIX}_db-sync-tmp:/tmp \
        -v ${STACK_PREFIX}_db-sync-data:/var/lib/cdbsync \
        -v ${STACK_PREFIX}_node-ipc:/node-ipc \
        -v ${STACK_PREFIX}_config:/config \
        $DB_SYNC_IMAGE \
        --config /config/db-sync-config.json \
        --socket-path /node-ipc/socket
}

assert_cardano_db_sync_exists() {

    start_db_sync_help() {
        docker start ${STACK_PREFIX}_cardano-postgres
        wait_for_psql_ready
        docker start ${STACK_PREFIX}_cardano-db-sync
    }

    run_db_sync_help() {
        db_sync_run
    }

    if_container_stopped ${STACK_PREFIX}_cardano-db-sync start_db_sync_help  > /dev/tty

    if_no_container ${STACK_PREFIX}_cardano-db-sync run_db_sync_help > /dev/tty
}

sql() {
    docker exec -i ${STACK_PREFIX}_cardano-postgres psql -U postgres cexplorer "$@"
}

db_sync_rm() {
    docker stop ${STACK_PREFIX}_cardano-db-sync
    docker rm ${STACK_PREFIX}_cardano-db-sync
    list ${STACK_PREFIX}_db-sync-tmp \
         ${STACK_PREFIX}_db-sync-data \
        | map λ vn . 'docker volume rm $vn'
}

node_run() {
    list ${STACK_PREFIX}_data \
         ${STACK_PREFIX}_config \
        | map λ vn . 'if_no_volume $vn docker volume create $vn'

    if [[ -z $NODE_SOCKET_DIR ]]; then
        if_no_volume ${STACK_PREFIX}_node-ipc docker volume create ${STACK_PREFIX}_node-ipc
    else
        mkdir -p $NODE_SOCKET_DIR
        if_no_volume ${STACK_PREFIX}_node-ipc \
                     docker volume create \
                     --driver local \
                     -o o=bind \
                     -o type=none \
                     -o device=$NODE_SOCKET_DIR \
                     ${STACK_PREFIX}_node-ipc
    fi

    if_no_container ${STACK_PREFIX}_cardano-node docker run \
		    --rm \
		    -v ${STACK_PREFIX}_config:/config \
		    $CONFIG_IMAGE

    if_no_container ${STACK_PREFIX}_cardano-node docker run \
		    -d \
		    --name ${STACK_PREFIX}_cardano-node \
		    -v ${STACK_PREFIX}_node-ipc:/opt/cardano/ipc \
		    -v ${STACK_PREFIX}_data:/opt/cardano/data \
		    -v ${STACK_PREFIX}_config:/opt/cardano/config \
		    -p $NODE_PORT:3001 \
		    $NODE_IMAGE \
		    run \
		    --config /opt/cardano/config/node-config.json \
		    --topology /opt/cardano/config/topology.json \
		    --port 3001
}

db_sync_rm() {
    docker stop ${STACK_PREFIX}_cardano-db-sync
    docker rm ${STACK_PREFIX}_cardano-db-sync
    docker stop ${STACK_PREFIX}_cardano-postgres
    docker rm ${STACK_PREFIX}_cardano-postgres
    list ${STACK_PREFIX}_cardano-postgres \
         ${STACK_PREFIX}_db-sync-tmp \
         ${STACK_PREFIX}_db-sync-data \
        | map λ vn . 'docker volume rm $vn'
}

node_rm() {
    docker stop ${STACK_PREFIX}_cardano-node
    docker rm ${STACK_PREFIX}_cardano-node
    list ${STACK_PREFIX}_data ${STACK_PREFIX}_node-ipc ${STACK_PREFIX}_config \
        | map λ vn . 'docker volume rm $vn'
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

    docker start ${STACK_PREFIX}_cardano-node

EOF

	      local key
	      read -p "I can start it for you [y/n]: " -n1 key < /dev/tty
	      echo
	      if [[ $key == 'y' ]]; then
	          docker start ${STACK_PREFIX}_cardano-node > /dev/null
	          ask_for_continuation
	      else
	          echo
	          exit 2
        fi
    }

    run_node_help() {


        emit_create_socket_dir() {
            [[ -z $NODE_SOCKET_DIR ]] || echo "mkdir -p $NODE_SOCKET_DIR"
        }

        emit_create_node_ipc() {
            if [[ -z $NODE_SOCKET_DIR ]]; then
                echo "docker volume create ${STACK_PREFIX}_node-ipc"
            else
                echo "docker volume create --driver local -o o=bind -o type=none -o device=$NODE_SOCKET_DIR ${STACK_PREFIX}_node-ipc"
            fi
        }

	      cat <<EOF
Cardano node does not exists, run one using following commands:

    docker volume create ${STACK_PREFIX}_data
    $(emit_create_socket_dir)
    $(emit_create_node_ipc)
    docker volume create ${STACK_PREFIX}_config

    docker run \\
      --rm \\
      -v ${STACK_PREFIX}_config:/config \\
      $CONFIG_IMAGE

    docker run \\
      -d \\
      --name ${STACK_PREFIX}_cardano-node \\
      -v ${STACK_PREFIX}_node-ipc:/opt/cardano/ipc \\
      -v ${STACK_PREFIX}_data:/opt/cardano/data \\
      -v ${STACK_PREFIX}_config:/opt/cardano/config \\
      -p $NODE_PORT:3001 \\
      $NODE_IMAGE \\
      run \\
      --config /opt/cardano/config/node-config.json \\
      --topology /opt/cardano/config/topology.json \\
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

    if_container_stopped ${STACK_PREFIX}_cardano-node start_node_help  > /dev/tty

    if_no_container ${STACK_PREFIX}_cardano-node run_node_help > /dev/tty
}

wallet_cli() {
  docker run \
    -i \
    --rm $WALLET_IMAGE "$@"
}

node_cli() {
    docker run \
	   --name node-cli \
	   --rm \
	   --entrypoint cardano-cli \
	   -e NETWORK=testnet \
	   -e CARDANO_NODE_SOCKET_PATH=$NODE_CONTAINER_SOCKET_PATH \
	   -v ${STACK_PREFIX}_node-ipc:/ipc \
	   -v ${sandbox_dir}:/out \
	   $NODE_IMAGE "$@"
}

new_sandbox() {
    local path=$SANDBOX_ROOT_DIR/$(date +"%Y%m%dT%H%M%S")
    mkdir -p $path
    echo $path
}

get_script_addr() {
    local file_name=$1  
    node_cli address build --payment-script-file /out/$file_name $NETWORK
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


pretty_print_utxo() {
    cnt=0; 
    while read line; do 
      cnt=$((cnt+1))
      tx=$(echo $line | jq -r ".tx")
      addr=$(echo $line | jq -r ".address")
      tokens=$(echo $line | jq -r ".value" | tokens_from_value | mappend_value | foldl lambda acc a . 'echo $acc+$a' | trim)
      lovelace=$(echo $line | jq -r ".value.lovelace" | trim)
      echo -e "[$cnt]\t$tx\t$lovelace |\t$tokens"
    done < <(cat - | jq -c ".[]")
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
	get_utxos_filtered | pretty_print_utxo > /dev/tty
	read  -p '> ' n < /dev/tty
	if [[ $(seq 1 $cnt | grep -e "^$n\$" | wc -l) -eq 0 ]]; then
	    echo 'No such element, choose again...' > /dev/tty
	    get_utxo $addr
	else
	    list $utxos | drop $((n-1)) | head -1
	fi
    fi
}

mappend_value() {
  cvalue=0
  casset=  
  while read line; do
    # echo "L == $line"
    curr=$(echo $line | cut -d' ' -f1)
    token=$(echo $line | cut -d' ' -f2)
    value=$(echo $line | cut -d' ' -f3)
    asset="$curr.$token"

    if [[ $casset == $asset ]]; then
      cvalue=$((value+cvalue))
    else
      [[ ! $cvalue -eq 0 ]] && echo "$cvalue $casset"
      casset=$asset
      cvalue=$value
    fi
  done < <(cat - | sort)
  [[ ! $cvalue -eq 0 ]] && echo "$cvalue $casset"
}

get_utxo_value2_at_tx() {
    local addr=$1
    local tx=$2
    get_utxos $addr | filter_utxo_by_tx $tx | jq -r ".value"
}

tokens_from_value() {
  jq -r ". as \$v | keys | map(. as \$c | select(\$c!=\"lovelace\") | \$v[\$c] | keys | map(. as \$t | [\$c, \$t, \$v[\$c][]] | @tsv)  )" \
    | jq -r ".[][]"
}

get_utxo_tsv_value_at_tx() {
  local addr=$1
  local tx=$2  
  get_utxo_value2_at_tx $addr $tx | tokens_from_value
}

get_utxo_currencies_at_tx() {
    local addr=$1
    local tx=$2
    get_utxos $addr | filter_utxo_by_tx $tx | jq -r ".value | keys"
}

# TODO change name to get_utxo_lovelace_at_tx
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

gen_uuid() {
  cat /proc/sys/kernel/random/uuid
}

submit_tx() {
  read -n1 -p 'Submit transaction [y/n] > ' ans < /dev/tty
  echo
  if [[ $ans == 'y' ]]; then
    (1>/dev/tty echo -e "\nSubmiting transaction...\n")
    node_cli transaction submit \
      --tx-file /out/tx.signed $NETWORK
    ret "true"  
  else
    ret "false"  
  fi
}

sign_tx() {
  local keys=$(list $@ | map lambda a . 'echo --signing-key-file /out/$a' | unlist)
  node_cli transaction sign \
    --tx-body-file /out/tx.draft $keys \
    --out-file /out/tx.signed $NETWORK
}
