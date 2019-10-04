#!/usr/bin/env bash

echo "*******************REED************************"
echo "Starting run_get_test_files.sh"

date

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]]; then
  echo "arg1 - GitHub SLUG"
  echo "arg2 - Number of rounds"
  echo "arg3 - Timeout in seconds"
  exit
fi

slug=$1
rounds=$2
timeout=$3

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running the get test files tools"

idflakiesSha=$( git rev-parse HEAD )
echo $idflakiesSha

date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}

# Incorporate tooling into the project, using Java XML parsing
/home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .

# Step 1 : Run the entire test suite and save all tests' file
timeout ${timeout}s /home/awshi2/apache-maven/bin/mvn testrunner:testplugin ${MVNOPTIONS} -Dtestplugin.className=edu.illinois.cs.dt.tools.utility.GetTestFilePlugin -fn -B -e |& tee get-test-file.log

cp get-test-file.log ${RESULTSDIR}

# Redirect to a different name in case the test-to-file.csv is already in .
# This is necessary because cat cannot read and redirect output to the same file
find . -name test-to-file.csv | xargs cat > ./test-to-file-temp.csv
mv -f test-to-file-temp.csv test-to-file.csv

cp /home/awshi2/$slug/test-to-file.csv ${RESULTSDIR}

echo "*******************REED************************"
echo "Finished run_get_test_files.sh"
date
