#!/bin/bash

if [[ $1 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    echo "arg2 - Script arguments (Optional)"
    exit
fi

mkdir -p "logs"
fname="logs/$(basename $1 .csv)-log.txt"

echo "Logging to $fname"
bash create_and_run_dockers-sha-finding.sh "$@" &> $fname

DIRECTORY=$(basename $1 .csv)
if [ -d "$DIRECTORY" ]; then
    # Will enter here if $DIRECTORY exists, even if it contains spaces
    yes | cp -rf $fname $DIRECTORY/
fi

echo "Finished running $fname"

