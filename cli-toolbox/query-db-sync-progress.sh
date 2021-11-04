#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

q_progress() {
    cat <<EOF
    select 100 * 
      (
        extract (epoch from (max (time) at time zone 'UTC')) - 
        extract (epoch from (min (time) at time zone 'UTC'))
      ) 
        / 
      (
         extract (epoch from (now () at time zone 'UTC')) - 
         extract (epoch from (min (time) at time zone 'UTC'))
      ) as sync_percent from block
EOF
}

q_progress | sql

echo 'select now () - max (time) as behind_by from block' | sql

echo "select pg_size_pretty (pg_database_size ('cexplorer'))" | sql