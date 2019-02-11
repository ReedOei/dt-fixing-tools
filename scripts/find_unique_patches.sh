#!/bin/bash

if [[ $1 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    exit
fi

debuggingresults=$1

IFS=$'\n'
#for m in $(find ${debuggingresults} -name "*.patch.*" | xargs -n1 basename | sed 's;\.patch.*;;' | sort -u); do
for proj in $(find ${debuggingresults} -name "fixer.log" | xargs -n1 dirname | xargs -n1 basename | cut -d'=' -f1 | sort -u); do
    # Make a temporary file but manipulate it through a file descriptor, so it gets deleted regardless when script ends
    tmpfile=$(mktemp /tmp/find_unique_patches.XXXXXX)
    exec 3> "${tmpfile}"
    rm "${tmpfile}"

    testcount=0
    pairscount=0
    count=0
    for d in $(find ${debuggingresults} -name "${proj}=*_output"); do
        # Count how many dependent tests there are
        if [[ $(grep -r "INLINE" ${d}) != "" ]]; then
            testcount=$((testcount + 1))
        fi

        # Count unique patches
        for p in $(find ${d} -name "*.patch*"); do
            if [[ $(grep "NO CLEANERS" ${p}) == "" ]]; then
                pairscount=$((pairscount + 1))
            fi
            if [[ $(grep "INLINE" ${p}) != "" ]]; then
                count=$((count + 1))
                linenum=$(grep -n "============" ${p} | cut -d':' -f1)
                modified=$(grep -h "MODIFIED" ${p})
                # Checking both patch contents and the name of the modified method
                echo $(md5sum <(echo "$(echo ${modified}) $(tail -n +$((linenum + 1)) ${p})") | cut -d' ' -f1) >>3
            fi
        done
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${proj}_unique_patches}{N/A}"
        echo "\\Def{${proj}_unique_patches_percentage}{N/A}"
    else
        echo "\\Def{${proj}_unique_patches}{$(sort -u <3 | wc -l)}"
        echo "\\Def{${proj}_unique_patches_percentage}{$(echo "$(sort -u <3 | wc -l) / ${count}" | bc -l | xargs printf "%.2f")}"
    fi
    echo "\\Def{${proj}_fixed_tests}{${testcount}}"
    echo "\\Def{${proj}_working_patches}{${count}}"
    echo "\\Def{${proj}_possible_patches}{${pairscount}}"
    echo -n "" >3
done
