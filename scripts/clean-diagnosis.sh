#!/usr/bin/env bash

# Usage: bash clean-diagnosis.sh [PATH]
# Run from the module you want to clean (or supply the path to it). Will deleted everything except for detection results.

set -e -x

if [[ ! -z "$1" ]]; then
    cd "$1"
fi

if [[ ! -d ".dtfixingtools" ]]; then
    echo "Error: No .dtfixingtools here!"
    exit 1
fi

rm -rf ".dtfixingtools/minimized"
rm -rf ".dtfixingtools/pollution-data"
rm -rf ".dtfixingtools/diagnosis"
rm -rf ".dtfixingtools/static-field-info-FIRST_ACCESS"
rm -rf ".dtfixingtools/static-field-info-TRACK"
rm -rf ".dtfixingtools/static-field-info-REWRITE"

