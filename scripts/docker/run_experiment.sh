#!/bin/bash

# This script is the entry point script that is run inside of the Docker image
# for running the experiment for a single project

if [[ $1 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    exit
fi

slug=$1

date

# Update all tooling
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/update.sh"

# Start the run_project.sh script using the awshi2 user
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/run_project.sh ${slug}"

# Change permissions of results and copy outside the Docker image (assume outside mounted under /Scratch)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
chown -R $(id -u):$(id -g) /home/awshi2/output/
cp -r /home/awshi2/output/ /Scratch/${modifiedslug}_output/
