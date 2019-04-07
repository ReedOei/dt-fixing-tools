#!/usr/bin/env bash

echo "*******************REED************************"
echo "Starting run_concurrency_tools.sh"

git rev-parse HEAD
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
testName=$4
className=$( echo ${testName} | rev | cut -d . -f 2- | rev )

# Setup RV-Predict
cd /home/awshi2/
wget "http://winglam2.web.engr.illinois.edu/personal/RV-Predict.zip" -O "/home/awshi2/RV-Predict.zip"
unzip "RV-Predict.zip"

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Set environment up, just in case
source ~/.bashrc

# Incorporate tooling into the project, using Java XML parsing
# TODO: Update the modify-script to allow adding other classes. This would allow us to simplify the line below that calls the testrunner
cd /home/awshi2/${slug}
/home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh . rvPredict ${testName} /home/awshi2/RV-Predict/rv-predict.jar

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running the concurrency tools"
git rev-parse HEAD
date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

timeout ${timeout}s /home/awshi2/apache-maven/bin/mvn test ${MVNOPTIONS} -Dtest=${className} -DfailIfNoTests=false -fn -B -e |& tee concurrency.log


# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}

for logs in $(find . -name "logs" -type d); do (if [[ $logs != *".git"* ]]; then echo $logs; fi); done | xargs -I {} mv {} /home/awshi2/output/

/home/awshi2/dt-fixing-tools/scripts/gather-results $(pwd) ${RESULTSDIR}
mv concurrency.log ${RESULTSDIR}
mv /home/awshi2/mvn-test.log ${RESULTSDIR}
mv /home/awshi2/mvn-test-time.log ${RESULTSDIR}

echo "*******************REED************************"
echo "Finished run_concurrency_tools.sh"
date

