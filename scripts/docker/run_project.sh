#!/bin/bash

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    exit
fi

slug=$1

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Set environment up, just in case
source ~/.bashrc

# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}
/home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .

# Run the plugin
/home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -fn |& tee log

# Gather the results, put them up top
mkdir /home/awshi2/output/
/home/awshi2/dt-fixing-tools/scripts/gather-results $(pwd) /home/awshi2/output/
