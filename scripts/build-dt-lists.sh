#!/usr/bin/env bash

if [[ -z "$1" ]] || [[ -z "$2" ]] || [[ -z "$3" ]] ; then
    echo "Usage: bash build-dt-lists.sh FULL_DT_LIST DATASET OUTPUT_LOCATION"
    exit 1
fi

full_dt_list="$1"
dataset="$2"
output="$3"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

if [[ ! "$full_dt_list" =~ "$/" ]]; then
    full_dt_list="$(cd "$(dirname $full_dt_list)"; pwd)/$(basename $full_dt_list)"
fi

if [[ ! "$dataset" =~ "$/" ]]; then
    dataset="$(cd "$(dirname $dataset)"; pwd)/$(basename $dataset)"
fi

if [[ ! "$output" =~ "$/" ]]; then
    output="$(cd "$(dirname $output)"; pwd)/$(basename $output)"
fi

# Go to where the pom is
cd "$scripts_folder/.."

mvn install exec:java -Dexec.mainClass="edu.illinois.cs.dt.tools.analysis.BuildDTLists" \
    -Dexec.args="--full-dt-list '$full_dt_list' --dataset '$dataset' --output '$output'"
