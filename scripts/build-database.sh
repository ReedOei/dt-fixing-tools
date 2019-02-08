#!/usr/bin/env bash

# Usage: bash build-database.sh RESULTS_FOLDER DATABASE SUBJECT_LIST

set -e

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] || [[ $4 == "" ]]; then
    echo "arg1 - Results folder"
    echo "arg2 - Path to database to create"
    echo "arg3 - List of subjects/shas in CSV (format: url,sha)"
    echo "arg4 - List of subjects/shas/loc/testloc in CSV (format: url,sha,loc,testloc)"
    exit
fi

results_folder="$1"
database="$2"
subject_list="$3"
subject_list_loc="$4"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

if [[ ! "$results_folder" =~ "$/" ]]; then
    results_folder="$(cd $results_folder; pwd)"
fi

if [[ ! "$database" =~ "$/" ]]; then
    database="$(cd "$(dirname $database)"; pwd)/$(basename $database)"
fi

if [[ ! "$subject_list" =~ "$/" ]]; then
    subject_list="$(cd "$(dirname $subject_list)"; pwd)/$(basename $subject_list)"
fi

if [[ ! "$subject_list_loc" =~ "$/" ]]; then
    subject_list_loc="$(cd "$(dirname $subject_list_loc)"; pwd)/$(basename $subject_list_loc)"
fi

# Go to where the pom is
cd "$scripts_folder/.."

mvn install -DskipTests exec:java \
    -Dexec.mainClass="edu.illinois.cs.dt.tools.analysis.Analysis" \
    -Dexec.args="--results '$results_folder' --db '$database' --subjectList '$subject_list' --subjectListLoc '$subject_list_loc'"

# Now that we know the flaky tests, we want to mark whether they occurred in a class with @FixMethodOrder
# They should have already all been downloaded above, so we just need to cd and check
bash "$scripts_folder/update-fix-method-order.sh" "$database" "$subject_list"

cat "$scripts_folder/../src/main/sql/build/update_od_classification.sql" | sqlite3 "$database"
