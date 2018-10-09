#!/usr/bin/env bash

# Usage: bash generate_graphic.sh <DATABASE> <SUBJECT NAME>

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

bash "$scripts_folder/list_flaky_runs.sh" "$1" "$2"
"$scripts_folder"/../graphic
