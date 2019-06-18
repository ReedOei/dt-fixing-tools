#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "Usage: ./gather-results.sh <path> <output path>"
    exit
fi

path=$1
outputpath=$2

for d in $(find ${path} -name .dtfixingtools); do
    modulekey=$(echo $(basename ${path})-$(realpath $(dirname ${d}) --relative-to ${path} | sed 's;/;-;g'))
    cp -r ${d} ${outputpath}/${modulekey}
done
