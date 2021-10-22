#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

q_tx_inputs() {
    cat <<EOF
    select  json_agg(row_to_json(tx_out)) from tx_out
      inner join tx_in on tx_out.tx_id = tx_in.tx_out_id
      inner join tx on tx.id = tx_in.tx_in_id and tx_in.tx_out_index = tx_out.index
      where tx.hash = '\x$1'
EOF
}

q_tx_inputs $1 | sql -t | jq