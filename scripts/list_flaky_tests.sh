#!/usr/bin/env bash

# Usage: bash list_flaky_tests.sh <database> [subject_name]

database="$1"
subject_name="$2"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)
procedure_file="$scripts_folder/../src/main/sql/list_flaky_tests_for_subject.sql"

# NOTE: If you change :name, then you will also need to change src/main/sql/list_flaky_tests_for_subject.sql
procedure="$(cat $procedure_file | sed -E "s/:name/$subject_name/g")"

echo "$procedure" | sqlite3 -header -column "$database"
