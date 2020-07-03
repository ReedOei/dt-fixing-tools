#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

# For everything together
overallendtoendtime=0
overallendtoendcount=0

# For successful patch only
overallendtoendsometime=0
overallendtoendsomecount=0

# For failure to patch only
overallendtoendnonetime=0
overallendtoendnonecount=0

IFS=$'\n'
for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    # Keep track of some stats per module
    rollingsum=0
    count=0
    rollingsumsome=0
    countsome=0
    rollingsumnone=0
    countnone=0

    # Start with finding first polluter (meaning test is a victim)
    for t in $(grep ",${module}" ${testsfile} | grep ",victim" | grep -v "#" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/")
        # Find first polluter
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST POLLUTER" ${f})
            pollutertime=$(echo ${l} | cut -d']' -f3 | cut -d' ' -f7)

            # See if there is a cleaner
            cf=$(find $(find $(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/" | xargs -n1 dirname) -name minimized) -name "*.json" | head -1)
            if [[ ${cf} != "" ]]; then
                for result in $(python old-python-scripts/find_first_cleaner.py ${cf} | head -1); do
                    time=$(echo ${result} | cut -d',' -f1)
                    iscleaner=$(echo ${result} | cut -d',' -f2)
                    if [[ ${iscleaner} == 'SUCCESS' ]]; then
                        cleanertime=${time}

                        # Look for the time to make the first patch
                        l=$(grep -h "FIRST PATCH" ${f})
                        if [[ ${l} != "" ]]; then
                            patchtime=$(echo ${l} | cut -d' ' -f12)

                            # Now add it all up
                            rollingsum=$(echo "${rollingsum} + ${pollutertime} + ${cleanertime} + ${patchtime}" | bc -l)
                            count=$((count + 1))
                            rollingsumsome=$(echo "${rollingsumsome} + ${pollutertime} + ${cleanertime} + ${patchtime}" | bc -l)
                            countsome=$((countsome + 1))
                        fi
                    else
                        cleanertime=${time}

                        # Add it all up, without non-existant time to make patch
                        rollingsum=$(echo "${rollingsum} + ${pollutertime} + ${cleanertime}" | bc -l)
                        count=$((count + 1))
                        rollingsumnone=$(echo "${rollingsumnone} + ${pollutertime} + ${cleanertime}" | bc -l)
                        countnone=$((countnone + 1))
                    fi
                done
            fi

        fi
    done

    # Look for first setter (meaning test is a brittle)
    for t in $(grep ",${module}" ${testsfile} | grep ",brittle" | grep -v "#" | cut -d',' -f1); do
        f=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/")
        if [[ ${f} != "" ]]; then
            l=$(grep -h "FIRST POLLUTER" ${f})
            settertime=$(echo ${l} | cut -d']' -f3 | cut -d' ' -f7)

            # See if there is a patch
            l=$(grep -h "FIRST PATCH" ${f})
            if [[ ${l} != "" ]]; then
                patchtime=$(echo ${l} | cut -d' ' -f12)

                # Now add it all up
                rollingsum=$(echo "${rollingsum} + ${settertime} + ${patchtime}" | bc -l)
                count=$((count + 1))
                rollingsumsome=$(echo "${rollingsumsome} + ${settertime} + ${patchtime}" | bc -l)
                countsome=$((countsome + 1))
            fi
        fi
    done

    # Report average time per module
    if [[ ${count} != 0 ]]; then
        echo "\\Def{${module}_firstoverall_time}{$(echo "${rollingsum} / ${count}" | bc -l | xargs printf "%'.0f")}"
        overallendtoendtime=$(echo "${overallendtoendtime} + ${rollingsum}" | bc -l)
        overallendtoendcount=$(echo "${overallendtoendcount} + ${count}" | bc -l)
    fi
    if [[ ${countsome} != 0 ]]; then
        echo "\\Def{${module}_firstoverallsome_time}{$(echo "${rollingsumsome} / ${countsome}" | bc -l | xargs printf "%'.0f")}"
        overallendtoendsometime=$(echo "${overallendtoendsometime} + ${rollingsumsome}" | bc -l)
        overallendtoendsomecount=$(echo "${overallendtoendsomecount} + ${countsome}" | bc -l)
    fi
    if [[ ${countnone} != 0 ]]; then
        echo "\\Def{${module}_firstoverallnone_time}{$(echo "${rollingsumnone} / ${countnone}" | bc -l | xargs printf "%'.0f")}"
        overallendtoendnonetime=$(echo "${overallendtoendnonetime} + ${rollingsumnone}" | bc -l)
        overallendtoendnonecount=$(echo "${overallendtoendnonecount} + ${countnone}" | bc -l)
    fi
done

echo "\\Def{average_firstoverall_time}{$(echo "${overallendtoendtime} / ${overallendtoendcount}" | bc -l | xargs printf "%'.0f")}"
echo "\\Def{average_firstoverallsome_time}{$(echo "${overallendtoendsometime} / ${overallendtoendsomecount}" | bc -l | xargs printf "%'.0f")}"
echo "\\Def{average_firstoverallnone_time}{$(echo "${overallendtoendnonetime} / ${overallendtoendnonecount}" | bc -l | xargs printf "%'.0f")}"
