#!/bin/bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $dir/etc/config
source $dir/lib/fun.sh
source $dir/lib/lib.sh

show_help() {
  cat << EOF

  Usage: ${0##*/} [-h]

  Application description.
 
  -h                     Print this help.

EOF
}

while getopts "h" opt; do
    
    case $opt in
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

required=()

for req in ${required[@]}; do
  [[ -z ${!req} ]] && echo && echo "  Please specify $req" && show_help &&  exit 1
done

node_rm