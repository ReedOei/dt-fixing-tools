#!/usr/bin/env bash

echo "*******************REED************************"
echo "Starting run_concurrency_tools.sh"

date

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] || [[ $4 == "" ]] || [[ $5 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Number of rounds"
    echo "arg3 - Timeout in seconds"
    echo "arg4 - Test name"
    echo "arg5 - Type of concurrency tool to run"
    exit
fi

slug=$1
rounds=$2
timeout=$3
testName=$4
toolName=$5
className=$( echo ${testName} | rev | cut -d . -f 2- | rev )

# Setup concurrency tool
cd /home/awshi2/

if [[ $toolName == "rvPredict" ]]; then
    # Setup RV-Predict
    wget "http://winglam2.web.engr.illinois.edu/personal/RV-Predict.zip" -O "/home/awshi2/RV-Predict.zip"
    unzip "RV-Predict.zip"
elif [[ $toolName == "wiretap" ]]; then
    # Setup wiretap
    git clone https://github.com/ucla-pls/wiretap.git
    cd wiretap
    ant
else
    echo "Unknown toolName. Arguments passed to run_concurrency_tools.sh are: $1 | $2 | $3 | $4 | $5"
    exit
fi

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Set environment up, just in case
source ~/.bashrc

# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}

if [[ $toolName == "rvPredict" ]]; then
    /home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh . rvPredict ${testName} /home/awshi2/RV-Predict/Java/lib/rv-predict.jar
elif [[ $toolName == "wiretap" ]]; then
    /home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh . wiretap ${testName} /home/awshi2/wiretap/build/wiretap.jar
else
    echo "Unknown toolName. Arguments passed to run_concurrency_tools.sh are: $1 | $2 | $3 | $4 | $5"
    exit
fi

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running the concurrency tools"
git rev-parse HEAD
date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

echo 'Running: mvn test ${MVNOPTIONS} -Dtest="${className}#${testName}" -DfailIfNoTests=false -fn -B -e'
timeout ${timeout}s /home/awshi2/apache-maven/bin/mvn test ${MVNOPTIONS} -Dtest="${className}#${testName}" -DfailIfNoTests=false -fn -B -e |& tee concurrency.log

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

