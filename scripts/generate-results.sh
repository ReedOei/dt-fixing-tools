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

echo "Starting generate results"
date

for f in $(ls rq$rqNum-sql/*.sql); do
    outputFile="$outputDir$(echo $f | cut -d/ -f2 | rev | cut -d. -f2- | rev).csv"
    echo "Input: $f ==== Output: $outputFile"
    sqlite3 -header -csv $database < $f > $outputFile
    dos2unix $outputFile
done

wc -l $outputDir/*


echo "Done generate results"
date

