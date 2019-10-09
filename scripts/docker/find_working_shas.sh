#!/usr/bin/env bash

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] || [[ $4 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Commit sha to start searching from"
    echo "arg3 - iDFlakies sha"
    echo "arg4 - Number of working commits needed"
  exit
fi

slug=$1
firstSha=$2
iDFlakiesSha=$3
commitsNeeded=$4

cd /home/awshi2/${slug}

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Identifying the first working sha"

idflakiesSha=$( git rev-parse HEAD )
echo $idflakiesSha

date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')

shortSha=${firstSha:0:7}
commitList=${RESULTSDIR}commitList-${shortSha}.txt
buildResults=${RESULTSDIR}built-shas-${shortSha}.csv

git log --reverse --pretty="format:%H" --ancestry-path ${firstSha}..${iDFlakiesSha} >${commitList}
currentListIndex=1
currentSha=$firstSha
commitsFound=0
entries=$(wc -l ${commitList} | cut -d' ' -f1)

while IFS= read -r currentSha; do
    shortSha=${currentSha:0:7}
    echo "$currentSha"
    echo "$shortSha"
    shaDistance=$(( ${entries} - ${currentListIndex} ))
    git checkout $currentSha
    timeout 1h /home/awshi2/apache-maven/bin/mvn clean compile -DskipTests -Dgpg.skip -B |& tee ${RESULTSDIR}${shortSha}-mvn-test.log
    if [[ "${PIPESTATUS[0]}" -ne 0 ]]; then
	echo "${currentSha},${shaDistance},BUILD_FAIL" >>${buildResults}
	git clean -x -d -f
	git checkout .
	((currentListIndex++))
	continue
    fi
    echo "${currentSha},${shaDistance},BUILD_SUCCESS" >>${buildResults}
    /home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .
    timeout 1h /home/awshi2/apache-maven/bin/mvn testrunner:testplugin ${MVNOPTIONS} -Dtestplugin.className=edu.illinois.cs.dt.tools.utility.GetTestFilePlugin -fn -B -e |& tee get-test-file-${shortSha}.log
    cp get-test-file-${shortSha}.log ${RESULTSDIR}
    git clean -x -d -f
    git checkout .
    ((commitsFound++))
    [ $commitsFound -ge $commitsNeeded ] && break
    ((currentListIndex++))
done <${commitList}

echo "*******************REED************************"
echo "Finished identifying the first working sha"
date
