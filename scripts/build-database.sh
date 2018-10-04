#!/usr/bin/env bash

# Usage: bash build-database.sh RESULTS_FOLDER DATABASE

results_folder="$1"
database="$2"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

# Go to where the pom is
cd "$scripts_folder/.."

if [[ -e "$database" ]]; then
    echo "$database already exists; overwrite (y/N)?"
    read choice
    if [[ "$choice" == "y" ]]; then
        rm -rf "$database"
    else
        exit 0
    fi
fi

mvn install exec:java -Dexec.mainClass="edu.illinois.cs.dt.tools.analysis.Analysis" -Dexec.args="--results '$results_folder' --db '$database'"
