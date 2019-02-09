#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

IFS=$'\n'
for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    # Do polluters (meaning test is a victim)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 2 -name fixer.log | grep "=${t}")
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST POLLUTER" ${f})
            time=$(echo ${l} | cut -d']' -f3 | cut -d' ' -f7)
            rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
            count=$((count + 1))
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_firstpolluter_time}{N/A}"
    else
        echo "\\Def{${module}_firstpolluter_time}{$(echo ${rollingsum} / ${count} | bc -l | xargs printf "%.2f")}"
    fi

    # Do setters (meaning test is a brittle)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",brittle" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 2 -name fixer.log | grep "=${t}")
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST POLLUTER" ${f})
            time=$(echo ${l} | cut -d']' -f3 | cut -d' ' -f7)
            rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
            count=$((count + 1))
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_firstsetter_time}{N/A}"
    else
        echo "\\Def{${module}_firstsetter_time}{$(echo ${rollingsum} / ${count} | bc -l | xargs printf "%.2f")}"
    fi

    # Do cleaners (meaning test is a victim)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | cut -d',' -f1); do
        f=$(find $(find $(find ${debuggingresults} -maxdepth 2 -name fixer.log | grep "=${t}" | xargs -n1 dirname) -name minimized) -name "*.json" | head -1)
        if [[ ${f} != "" ]]; then
            for time in $(python find_first_cleaner.py ${f}); do
                rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
                count=$((count + 1))
            done
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_firstcleaner_time}{N/A}"
    else
        echo "\\Def{${module}_firstcleaner_time}{$(echo ${rollingsum} / ${count} | bc -l | xargs printf "%.2f")}"
    fi

    # Do patches
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 2 -name fixer.log | grep "=${t}")
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST PATCH" ${f})
            if [[ ${l} != "" ]]; then
                time=$(echo ${l} | cut -d' ' -f12)
                rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
                count=$((count + 1))
            fi
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_firstpatch_time}{N/A}"
    else
        echo "\\Def{${module}_firstpatch_time}{$(echo ${rollingsum} / ${count} | bc -l | xargs printf "%.2f")}"
    fi
done
