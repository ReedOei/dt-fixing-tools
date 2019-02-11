#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

overallsetterstime=0
overallsetterscount=0
overallpolluterstime=0
overallpolluterscount=0
overallcleanerstime=0
overallcleanerscount=0

IFS=$'\n'
for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    # Do polluters (meaning test is a victim)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | grep -v "#" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}")
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
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_firstpolluter_time}{$(echo ${avgtime} | bc -l | xargs printf "%.2f")}"
        overallpolluterstime=$(echo ${avgtime} + ${overallpolluterstime} | bc -l)
        overallpolluterscount=$((overallpolluterscount + 1))
    fi

    # Do setters (meaning test is a brittle)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",brittle" | grep -v "#" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}")
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
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_firstsetter_time}{$(echo ${avgtime} | bc -l | xargs printf "%.2f")}"
        overallsetterstime=$(echo ${avgtime} + ${overallsetterstime} | bc -l)
        overallsetterscount=$((overallsetterscount + 1))
    fi

    # Do cleaners (meaning test is a victim)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | grep -v "#" | cut -d',' -f1); do
        f=$(find $(find $(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}" | xargs -n1 dirname) -name minimized) -name "*.json" | head -1)
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
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_firstcleaner_time}{$(echo ${avgtime} | bc -l | xargs printf "%.2f")}"
        overallcleanerstime=$(echo ${avgtime} + ${overallcleanerstime} | bc -l)
        overallcleanerscount=$((overallcleanerscount + 1))
    fi

    # Do patches
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep -v "#" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}")
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

# Output the overall times
echo "\\Def{average_firstpolluter_time}{$(echo ${overallpolluterstime} / ${overallpolluterscount} | bc -l | xargs printf "%.2f")}"
echo "\\Def{average_firstsetter_time}{$(echo ${overallsetterstime} / ${overallsetterscount} | bc -l | xargs printf "%.2f")}"
echo "\\Def{average_firstcleaner_time}{$(echo ${overallcleanerstime} / ${overallcleanerscount} | bc -l | xargs printf "%.2f")}"
