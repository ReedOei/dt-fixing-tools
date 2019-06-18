#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "Usage: ./gather-results.sh <path> <output path>"
    exit
fi

path=$1
outputpath=$2

modulekey() {
    projroot=$1
    moduledir=$2

    # In case it is not a subdirectory, handle it so does not use the .
    relpath=$(realpath $(dirname ${moduledir}) --relative-to ${projroot})
    if [[ ${relpath} == '.' ]]; then
        basename ${projroot}
        return
    fi

    # Otherwise convert into expected format
    echo $(basename ${projroot})-$(realpath $(dirname ${moduledir}) --relative-to ${projroot} | sed 's;/;-;g')
}

for d in $(find ${path} -name .dtfixingtools); do
    cp -r ${d} ${outputpath}/$(modulekey ${path} ${d})
done
