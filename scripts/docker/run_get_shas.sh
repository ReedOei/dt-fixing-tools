#!/usr/bin/env bash

echo "*******************REED************************"
echo "Starting run_get_shas.sh"

date

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] || [[ $4 == "" ]]; then
  echo "arg1 - GitHub SLUG"
  echo "arg2 - Number of rounds"
  echo "arg3 - Timeout in seconds"
  echo "arg4 - Test name"
  exit
fi

slug=$1
rounds=$2
timeout=$3
fullTestName=$4
fullClassName=$( echo ${fullTestName} | rev | cut -d . -f 2- | rev )
className=$( echo ${fullClassName} | rev | cut -d . -f 1 | rev )
testName="$( echo ${fullTestName} | rev | cut -d . -f 1 | rev )\s*\("


# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running the get shas tools"

idflakiesSha=$( git rev-parse HEAD )
echo $idflakiesSha

date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}

/home/awshi2/dt-fixing-tools/scripts/docker/s.sh /home/awshi2/commits.log ${testName} ${className} ${idflakiesSha}

cp /home/awshi2/commits.log ${RESULTSDIR}

mv mvn-test-time.log ${RESULTSDIR}
mv mvn-test.log ${RESULTSDIR}

echo "*******************REED************************"
echo "Finished run_get_shas.sh"
date
