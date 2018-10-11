#!/bin/bash

git rev-parse HEAD

date

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Number of rounds"
    exit
fi

slug=$1
rounds=$2

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Set environment up, just in case
source ~/.bashrc

# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}
/home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .

echo "*******************REED************************"
echo "Running testplugin for randomizemethods"
date

# Run the plugin, with timeout of 6 hours
timedout=0
timeout 6h /home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddt.randomize.rounds=${rounds} -fn -B |& tee log
if [ $? -eq 124 ]; then
    timedout=1
fi
# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/randomizemethods/
mkdir -p ${RESULTSDIR}
/home/awshi2/dt-fixing-tools/scripts/gather-results $(pwd) ${RESULTSDIR}
mv log ${RESULTSDIR}
if [[ ${timedout} == 1 ]]; then
    touch ${RESULTSDIR}/TIMEOUT # Mark a file when it times out
fi

echo "*******************REED************************"
echo "Running testplugin for randomizeclasses"
date

# Run the plugin with different random ordering
timedout=0
timeout 6h /home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddt.randomize.rounds=${rounds} -Ddetector.detector_type=random-class -fn -B |& tee log
if [ $? -eq 124 ]; then
    timedout=1
fi
# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/randomizeclasses/
mkdir -p ${RESULTSDIR}
/home/awshi2/dt-fixing-tools/scripts/gather-results $(pwd) ${RESULTSDIR}
mv log ${RESULTSDIR}
if [[ ${timedout} == 1 ]]; then
    touch ${RESULTSDIR}/TIMEOUT # Mark a file when it times out
fi

echo "*******************REED************************"
echo "Finished run_project.sh"
date

