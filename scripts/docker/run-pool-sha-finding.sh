#!/usr/bin/env bash

if [[ $1 == "" ]] || [[ $3 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    echo "arg2 - Number of processes to run at the same time (Optional)"
    echo "arg3 - Script arguments (Optional)"
    exit
fi

date

csvPath=$1
PROCESS_NUM="$2"
shift 2

if [[ -z "$PROCESS_NUM" ]]; then
    PROCESS_NUM="4"
fi

find "$csvPath" -maxdepth 1 -type f -name "*.csv" | xargs -P"$PROCESS_NUM" -I{} bash run-project-pool-sha-finding.sh {} "$@"

date
