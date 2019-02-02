#!/bin/bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - full path to directory with dataset of all logs (e.g., dataset-jan28-2019)"
    echo "arg2 - file to output csv results"
    exit
fi

datasetdir=$1
outputfile=$2

echo "#test,module,odclass,cleaner/setter,fixstatus" > ${outputfile}
for l in $(cat ${SCRIPTDIR}/../docker/data/fse19/list_od_test_type.csv | grep -v "#"); do
    t=$(echo ${l} | cut -d',' -f1)
    if [[ $(echo ${l} | cut -d',' -f3) == "brittle" ]]; then    # All brittles should have setter
        c=YES
    else
        f=$(find ${datasetdir} -name "${t}*.json" | grep "minimized" | head -1) # Look for minimized (getting only first right now)
        if [[ $(python parse_minimized_json_for_cleaners.py ${f}) == False ]]; then
            c=NO
        else
            c=YES
        fi
    fi
    f=$(find ${datasetdir} -name "${t}.patch*")  # Look for all patches
    if [[ ${f} == "" ]]; then
        echo ${l},${c},MISSING
        continue
    fi
    g=$(grep -lE "INLINE|PRIOR PATCH" ${f}) # It is considered fixed if it was INLINE or had a PRIOR PATCH
    if [[ ${g} == "" ]]; then
        echo ${l},${c},NOTFIX
    else
        echo ${l},${c},FIX
    fi
done >> ${outputfile}

# Check the output file for basic numbers
echo "NUM W/O activiti and spring-boot:" $(grep -v "#" ${outputfile} | grep -v "activiti" | grep -v "spring-boot" | wc -l)
echo "NUM FIXABLE W/O activiti and spring-boot:" $(grep -v "#" ${outputfile} | grep -v "activiti" | grep -v "spring-boot" | grep "YES," | wc -l)
echo "NUM FIXED W/O activiti and spring-boot:" $(grep -v "#" ${outputfile} | grep -v "activiti" | grep -v "spring-boot" | grep "YES," | grep ",FIX" | wc -l)
