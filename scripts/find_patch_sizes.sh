#!/bin/bash

if [[ $1 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    exit
fi

debuggingresults=$1

IFS=$'\n'
for t in $(find ${debuggingresults} -name "*.patch.*" | xargs -n1 basename | sed 's;\.patch.*;;' | sort -u); do
    rollingorigsum=0
    rollingnewsum=0
    count=0
    for p in $(find ${debuggingresults} -name "${t}.patch.*"); do
        if [[ $(grep "INLINE" ${p}) != "" ]]; then
            origsize=$(grep "ORIGINAL CLEANER SIZE: " ${p} | cut -d':' -f2 | xargs)
            newsize=$(grep "NEW CLEANER SIZE: " ${p} | cut -d':' -f2 | xargs)
            rollingorigsum=$((rollingnewsum + origsize))
            rollingnewsum=$((rollingnewsum + newsize))
            count=$((count + 1))
        fi
    done
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${t}_avg_patch_size_percentage}{N/A}"
        echo "\\Def{${t}_avg_patch_size}{N/A}"
    else
        echo "\\Def{${t}_avg_patch_size}{$(echo ${rollingnewsum} / ${count} | bc -l | xargs printf "%.2f")}"
        echo "\\Def{${t}_avg_patch_size_percentage}{$(echo "(${rollingnewsum} / ${rollingorigsum}) / ${count}" | bc -l | xargs printf "%.2f")}"
    fi
done
