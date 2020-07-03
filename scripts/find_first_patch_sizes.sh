#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

overallsize=0
overallsizeperc=0
overallcount=0

for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    rollingsize=0
    rollingsizeperc=0
    count=0

    for t in $(grep ",${module}" ${testsfile} | grep -v "#" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -name "*.json" | grep "=${t}/" | grep "minimized/")
        res=$(python old-python-scripts/compute_size_first_patch.py ${f})
        if [[ ${res} != "" ]]; then
            size=$(echo ${res} | cut -d',' -f1)
            rollingsize=$(echo "${size} + ${rollingsize}" | bc -l)
            sizeperc=$(echo ${res} | cut -d',' -f2)
            rollingsizeperc=$(echo "${sizeperc} + ${rollingsizeperc}" | bc -l)
            count=$((count + 1))
        fi
    done

    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_firstsize}{n/a}"
        echo "\\Def{${module}_firstsizeperc}{n/a}"
    else
        avgsize=$(echo "${rollingsize} / ${count}" | bc -l)
        echo "\\Def{${module}_firstsize}{$(echo "${avgsize}" | xargs printf "%'.1f")}"
        avgsizeperc=$(echo "${rollingsizeperc} / ${count} * 100" | bc -l)
        echo "\\Def{${module}_firstsizeperc}{$(echo "${avgsizeperc}" | xargs printf "%'.1f")\\%}"

        overallsize=$(echo "${overallsize} + ${rollingsize}" | bc -l)
        overallsizeperc=$(echo "${overallsizeperc} + ${rollingsizeperc}" | bc -l)
        overallcount=$(echo "${overallcount} + ${count}" | bc -l)
    fi
done

echo "\\Def{average_firstsize}{$(echo "${overallsize} / ${overallcount}" | bc -l | xargs printf "%'.1f")}"
echo "\\Def{average_firstsizeperc}{$(echo "(${overallsizeperc} * 100) / ${overallcount}" | bc -l | xargs printf "%'.1f")\\%}"
