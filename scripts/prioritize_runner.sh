#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

# Make a temporary file but manipulate it through a file descriptor, so it gets deleted regardless when script ends
tmpfile=$(mktemp /tmp/s.XXXXXX)
exec 3> "${tmpfile}"
rm "${tmpfile}"

# Get list of all victims and polluters
for f in $(find /home/reedoei2/issta-dataset/dataset-feb10-2019/ -name "*.json" | grep "minimized"); do python compute_sizes_per_patch.py ${f}; done >3

for module in $(grep -v "#" ${testsfile} | grep ",victim" | cut -d',' -f2 | sort -u); do
    python prioritize.py <(for t in $(grep ",${module}" ${testsfile} | grep -v "#" | grep ",victim" | cut -d',' -f1); do
        grep "^${t}," <3
    done) /home/reedoei2/issta-dataset/dataset-feb10-2019/
    echo "==========="
done
