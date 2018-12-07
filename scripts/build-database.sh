#!/usr/bin/env bash

# Usage: bash build-database.sh RESULTS_FOLDER DATABASE [MAX_TEST_RUNS]

results_folder="$1"
database="$2"
max_test_runs="$3"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

if [[ ! "$results_folder" =~ "$/" ]]; then
    results_folder="$(cd $results_folder; pwd)"
fi

if [[ ! "$database" =~ "$/" ]]; then
    database="$(cd "$(dirname $database)"; pwd)/$(basename $database)"
fi

if [[ -z "$max_test_runs" ]]; then
    max_test_runs="0"
fi

# Go to where the pom is
cd "$scripts_folder/.."

# TODO: MAKE THIS HANDLE INSERTING THE SUBJECT RAW
mvn install -DskipTests exec:java -Dexec.mainClass="edu.illinois.cs.dt.tools.analysis.Analysis" -Dexec.args="--results '$results_folder' --db '$database' --max-test-runs $max_test_runs"

