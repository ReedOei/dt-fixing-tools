#!/bin/bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if [[ $1 == "" ]]; then
    echo "arg1 - full path to directory with dataset of all logs (e.g., dataset-jan28-2019)"
    exit
fi

datasetdir=$1

echo "#victim,polluter,distance,distanceratio,sameclass"
for l in $(cat ${SCRIPTDIR}/../docker/data/fse19/list_od_test_type.csv | grep -v "#" | grep ",victim" | grep -v "activiti" | grep -v "spring-boot"); do
    t=$(echo ${l} | cut -d',' -f1)
    f=$(find ${datasetdir} -name "${t}*.json" | grep "minimized" | head -1) # Look for minimized (getting only first right now)
    python parse_minimized_json_for_polluter_distance.py ${f}
done
