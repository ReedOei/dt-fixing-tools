#!/usr/bin/env bash

# Usage: bash download-dt-list.sh
# Should be run from the root level of the github repo you are running on

if [[ $1 == "" ]]; then
    echo "arg1 - Test name"
    exit
fi

testName=$1

for pom_path in $(find -name "pom.xml"); do
    dir=$(dirname $pom_path)
    module_key=$(/home/awshi2/dt-fixing-tools/scripts/docker/module-key $(pwd) $dir)

    mkdir -p "$dir/.dtfixingtools/detection-results/"
    echo "[DOWNLOAD] Downloading $module_key to $dir/.dtfixingtools/detection-results/flaky-lists.json"

    cp for-minimizer/${testName}.json ${dir}/.dtfixingtools/detection-results/flaky-lists.json

    cp for-minimizer/${testName}-original ${dir}/.dtfixingtools/original-order
done

