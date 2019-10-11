#!/usr/bin/env bash

# Usage: bash generate-rq-results.sh DATABASE 

set -e

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - Path to existing database"
    echo "arg2 - RQ number"
    exit
fi

database="$1"
rqNum=$2

outputDir=rq$rqNum-results/
mkdir -p $outputDir

for f in $(ls rq$rqNum-sql/*.sql); do
    echo "Outputting to $outputDir$(echo $f | cut -d/ -f2 | rev | cut -d. -f2- | rev).csv"
    sqlite3 -csv $database < $f > "$outputDir$(echo $f | cut -d/ -f2 | rev | cut -d. -f2- | rev).csv"
done

wc -l $outputDir/*



