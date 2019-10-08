#!/bin/bash

# The input csvs are typically generated with the following command.
# for line in $(cat google-sheet-full.csv); do echo "$(echo $line | cut -d, -f 1-2),$(echo $line | cut -d, -f 4- | rev | cut -d, -f 3- | rev),$(echo $line | cut -d, -f 4)" >> full.csv ; done
# google-sheet-full.csv is from iDFlakies Google sheet

if [[ $1 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    exit
fi

OLDIFS="$IFS"
IFS=$'\n' # bash specific

git rev-parse HEAD
date

projfile=$1
useTestName=$2

if [[ $2 == "" ]]; then
    useTestName=true
fi


mkdir -p "individual-split"

for line in $(cat ${projfile}); do
    slug=$(echo ${line} | cut -d',' -f1 | rev | cut -d'/' -f1-2 | rev)
    sha=$(echo ${line} | cut -d',' -f2)
    testName=$(echo ${line} | cut -d',' -f 3- | rev | cut -d',' -f 2- | rev)

    modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
    short_sha=${sha:0:7}
    modifiedslug_with_sha="${modifiedslug}-${short_sha}"

    if [[ $testName == "" ]] || [[ "$useTestName" == false ]]; then
        folder_name=${modifiedslug_with_sha}=_output
    else
        folder_name=${modifiedslug_with_sha}=${testName}_output
    fi

    echo "$line" > individual-split/$folder_name.csv
done

IFS="$OLDIFS"
