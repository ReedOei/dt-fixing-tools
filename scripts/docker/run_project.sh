#!/bin/bash

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    exit
fi

slug=$1

# Set environment up, just in case
source ~/.bashrc

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Incorporate tooling into the project 
cd /home/awshi2/${slug}
/home/awshi2/dt-fixing-tools/scripts/add-detector-plugin.pl

# Run the plugin
/home/awshi2/apache-maven/bin/mvn testrunner:testplugin |& tee log

# Gather the results, put them up top
mkdir /home/awshi2/output/
/home/awshi2/dt-fixing-tools/scripts/gather-results $(pwd) /home/awshi2/output/
