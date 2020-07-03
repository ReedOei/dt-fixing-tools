#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

rollingstd=0
rollingstdcount=0

IFS=$'\n'

overallsize=0
overallsizeperc=0
overallsizecount=0
overallstd=0
overallstdcount=0

overallsizeonecount=0
overallcount=0

# Do some computation of sizes per module
for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    rollingsize=0
    rollingsizeperc=0
    count=0
    rollingstd=0
    rollingstdcount=0
    for t in $(grep ",${module}" ${testsfile} | grep -v "#" | cut -d',' -f1); do
        for p in $(find ${debuggingresults} -name "${t}.patch*"); do
            if [[ $(grep "INLINE" ${p}) != "" ]] || [[ $(grep "CLEANER DOES NOT FIX" ${p}) != "" ]] ; then
                origsize=$(grep "ORIGINAL CLEANER SIZE: " ${p} | cut -d':' -f2 | xargs)
                if [[ ${origsize} == "N/A" ]]; then
                    continue
                fi
                newsize=$(grep "NEW CLEANER SIZE: " ${p} | cut -d':' -f2 | xargs)
                sizeperc=$(echo "${newsize} / ${origsize}" | bc -l)
                rollingsize=$(echo "${rollingsize} + ${newsize}" | bc -l)
                rollingsizeperc=$(echo "${rollingsizeperc} + ${sizeperc}" | bc -l)
                count=$((count + 1))
                if [[ ${newsize} == 1 ]]; then
                    overallsizeonecount=$((overallsizeonecount + 1))
                fi
                overallcount=$((overallcount + 1))
            fi
        done
        # Standard deviation of sizes
        if [[ $(find ${debuggingresults} -name "${t}.patch*" | xargs grep "INLINE" | wc -l) -gt 1 ]]; then
            sizes=$(find ${debuggingresults} -name "${t}.patch*" | xargs grep -l "INLINE" | xargs grep -h "NEW CLEANER SIZE: " | cut -d':' -f2 | xargs | sed 's; ;,;g')
            std=$(echo "sizes=c(${sizes}); sd(sizes)" | R --no-save | grep "\[1\]" | cut -d' ' -f2)
            rollingstd=$(echo ${rollingstd} + ${std} | bc -l)
            rollingstdcount=$((rollingstdcount + 1))
        fi
    done

    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_avgsize}{n/a}"
        echo "\\Def{${module}_avgsizeperc}{n/a}"
    else
        avgsize=$(echo "${rollingsize} / ${count}" | bc -l)
        echo "\\Def{${module}_avgsize}{$(echo "${avgsize}" | xargs printf "%'.1f")}"
        avgsizeperc=$(echo "${rollingsizeperc} / ${count} * 100" | bc -l)
        echo "\\Def{${module}_avgsizeperc}{$(echo "${avgsizeperc}" | xargs printf "%'.1f")\\%}"
        overallsize=$(echo "${overallsize} + ${rollingsize}" | bc -l)
        overallsizeperc=$(echo "${overallsizeperc} + ${rollingsizeperc}" | bc -l)
        overallsizecount=$(echo "${overallsizecount} + ${count}" | bc -l)
    fi

    if [[ ${rollingstdcount} == 0 ]]; then
        echo "\\Def{${module}_stdsize}{n/a}"
    else
        avgstd=$(echo "${rollingstd} / ${rollingstdcount}" | bc -l)
        echo "\\Def{${module}_stdsize}{$(echo "${avgstd}" | xargs printf "%'.1f")}"
        overallstd=$(echo "${overallstd} + ${rollingstdcount}" | bc -l)
        overallstdcount=$(echo "${overallstdcount} + ${rollingstdcount}" | bc -l)
    fi

done

# Output the overall macros
echo "\\Def{average_avgsize}{$(echo "${overallsize} / ${overallsizecount}" | bc -l | xargs printf "%'.1f")}"
echo "\\Def{average_avgsizeperc}{$(echo "(${overallsizeperc} * 100) / ${overallsizecount}" | bc -l | xargs printf "%'.1f")\\%}"
echo "\\Def{average_stdsize}{$(echo "${overallstd} / ${overallstdcount}" | bc -l | xargs printf "%'.1f")}"
echo "\\Def{overall_sizeone}{$(echo "${overallsizeonecount}" | xargs printf "%'d")}"
echo "\\Def{overall_sizeone_perc}{$(echo "(${overallsizeonecount} / ${overallcount}) * 100" | bc -l | xargs printf "%'.1f")\\%}"
