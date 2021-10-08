#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} [-l] [-h]

  Application description.

  -l                     Query in loop 
  -h                     Print this help.

EOF
}

query_in_loop=0

while getopts "lh" opt; do
    
    case $opt in
	l)
	    query_in_loop=1
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

required=()

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

assert_cardano_node_exists

if [[ $query_in_loop -eq 1 ]]; then
    sqloop "Quering tip" 'get_tip | jq'
else
    get_tip | jq
fi

