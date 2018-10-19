#!/usr/bin/env bash

# Usage: bash build-dt-lists.sh FULL_DT_LIST DATASET
full_dt_list="$1"
dataset="$2"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

if [[ ! "$full_dt_list" =~ "$/" ]]; then
    full_dt_list="$(cd "$(dirname $full_dt_list)"; pwd)/$(basename $full_dt_list)"
fi

if [[ ! "$dataset" =~ "$/" ]]; then
    dataset="$(cd "$(dirname $dataset)"; pwd)/$(basename $dataset)"
fi

# Go to where the pom is
cd "$scripts_folder/.."

mvn install exec:java -Dexec.mainClass="edu.illinois.cs.dt.tools.analysis.BuildDTLists" \
    -Dexec.args="--full-dt-list '$full_dt_list' --dataset '$dataset'"
