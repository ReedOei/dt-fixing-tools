#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha,testName"
    echo "arg2 - Output path"
    exit
fi

projFile=$1
outputDir=$2

mkdir -p $outputDir

for line in $(cat ${projFile}); do
    slug=$(echo ${line} | cut -d',' -f1 | rev | cut -d'/' -f1-2 | rev | tr -s '/' | tr '/' '.')
    sha=$(echo ${line} | cut -d',' -f2)
    testName=$(echo ${line} | cut -d',' -f3)

    echo $line > $outputDir/${slug}=${testName}.csv
done
