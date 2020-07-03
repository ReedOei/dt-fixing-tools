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
overallpatchtime=0
overallpatchcount=0

IFS=$'\n'
for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    # Do polluters (meaning test is a victim)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | grep -v "#" | cut -d',' -f1); do
        # First get the time to the first polluter
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/")
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST POLLUTER" ${f})
            time=$(echo ${l} | cut -d']' -f3 | cut -d' ' -f7)
            rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
            count=$((count + 1))
        fi
        # Next, parse the corresponding minimized json to get all the other polluter times
        othertimes=$(python old-python-scripts/parse_minimized_for_nonfirst_polluter_times.py $(find ${debuggingresults} -name "${t}*.json" | grep "minimized"))
        rollingsum=$(echo ${rollingsum} + ${othertimes} | bc -l)
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_avgpolluter_time}{n/a}"
    else
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_avgpolluter_time}{$(echo ${avgtime} | bc -l | xargs printf "%'.0f")}"
        overallpolluterstime=$(echo "${overallpolluterstime} + ${rollingsum}" | bc -l)
        overallpolluterscount=$(echo "${overallpolluterscount} + ${count}" | bc -l)
    fi

    # Do setters (meaning test is a brittle)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",brittle" | grep -v "#" | cut -d',' -f1); do
        # First get the time to the first setter
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/")
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST POLLUTER" ${f})
            time=$(echo ${l} | cut -d']' -f3 | cut -d' ' -f7)
            rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
            count=$((count + 1))
        fi
        # Next, parse the corresponding minimized json to get all the other setter times
        othertimes=$(python old-python-scripts/parse_minimized_for_nonfirst_polluter_times.py $(find ${debuggingresults} -name "${t}*.json" | grep "minimized"))
        rollingsum=$(echo ${rollingsum} + ${othertimes} | bc -l)
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_avgsetter_time}{n/a}"
    else
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_avgsetter_time}{$(echo ${avgtime} | bc -l | xargs printf "%'.0f")}"
        overallsetterstime=$(echo "${overallsetterstime} + ${rollingsum}" | bc -l)
        overallsetterscount=$(echo "${overallsetterscount} + ${count}" | bc -l)
    fi

    # Do cleaners (meaning test is a victim)
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | grep -v "#" | cut -d',' -f1); do
        # Parse the minimized json to get all cleaner times
        f=$(find $(find $(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/" | xargs -n1 dirname) -name minimized) -name "*.json" | head -1)
        if [[ ${f} != "" ]]; then
            time=$(python old-python-scripts/find_all_cleanertimes.py ${f})
            if [[ ${time} != 0 ]]; then
                rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
                count=$((count + 1))
            fi
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_avgcleaner_time}{n/a}"
    else
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_avgcleaner_time}{$(echo ${avgtime} | bc -l | xargs printf "%'.0f")}"
        overallcleanerstime=$(echo "${overallcleanerstime} + ${rollingsum}" | bc -l)
        overallcleanerscount=$(echo "${overallcleanerscount} + ${count}" | bc -l)
    fi

    # Do patches
    rollingsum=0
    count=0
    for t in $(grep ",${module}" ${testsfile} | grep -v "#" | cut -d',' -f1); do
        # Parse the fix json to get the total time
        f=$(find $(find $(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/" | xargs -n1 dirname) -name fixer) -name "*.json" | head -1)
        if [[ ${f} != "" ]]; then
            time=$(python old-python-scripts/find_patch_time.py ${f})
            if [[ ${time} != 0 ]]; then
                rollingsum=$(echo ${rollingsum} + ${time} | bc -l)
                count=$((count + 1))
            fi
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_avgpatch_time}{n/a}"
    else
        avgtime=$(echo ${rollingsum} / ${count})
        echo "\\Def{${module}_avgpatch_time}{$(echo ${avgtime} | bc -l | xargs printf "%'.0f")}"
        overallpatchtime=$(echo "${overallpatchtime} + ${rollingsum}" | bc -l)
        overallpatchcount=$(echo "${overallpatchcount} + ${count}" | bc -l)
    fi
done

# Output the overall times
echo "\\Def{average_avgpolluter_time}{$(echo ${overallpolluterstime} / ${overallpolluterscount} | bc -l | xargs printf "%'.0f")}"
echo "\\Def{average_avgsetter_time}{$(echo ${overallsetterstime} / ${overallsetterscount} | bc -l | xargs printf "%'.0f")}"
echo "\\Def{average_avgcleaner_time}{$(echo ${overallcleanerstime} / ${overallcleanerscount} | bc -l | xargs printf "%'.0f")}"
echo "\\Def{average_avgpatch_time}{$(echo ${overallpatchtime} / ${overallpatchcount} | bc -l | xargs printf "%'.0f")}"
