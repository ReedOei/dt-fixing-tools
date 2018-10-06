#!/usr/bin/env bash

# Usage: bash list_flaky_tests.sh <database>

database="$1"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)
sqlite3 -header -column "$database" < "$scripts_folder/../src/main/sql/list_flaky_tests_for_subject.sql"
