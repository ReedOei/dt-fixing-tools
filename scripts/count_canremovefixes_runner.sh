#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    jsonfiles=""
    for t in $(grep ",${module}" ${testsfile} | grep -v "#" | cut -d',' -f1); do
        d=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/" | xargs -n1 dirname)
        json=$(find ${d} -name "*.json" | grep "/fixer/")
        jsonfiles="${jsonfiles} ${json}"
    done
    echo "${module} $(python count_canremovefixes.py ${jsonfiles})"
done
