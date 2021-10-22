#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

q_tx_out() {
    cat <<EOF
    select json_agg(row_to_json(tx_out)) from tx_out 
      inner join tx on tx_out.tx_id = tx.id 
      where tx.hash = '\x$1'
EOF
}

q_tx_out $1 | sql -t | jq