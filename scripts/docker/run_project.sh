#!/bin/bash

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    exit
fi

slug=$1

# Setup prolog stuff
cd ${HOME}/dt-fixing-tools/scripts/
./setup

# Incorporate tooling into the project 
cd ${HOME}/${slug}
${HOME}/dt-fixing-tools/scripts/add-detector-plugin.pl

# Run the plugin
mvn testrunner:testplugin |& tee log

# Gather the results, put them up top
mkdir ${HOME}/output/
${HOME}/dt-fixing-tools/scripts/gather-results $(pwd) ${HOME}/output/
