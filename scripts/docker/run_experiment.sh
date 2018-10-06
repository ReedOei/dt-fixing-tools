#!/bin/bash

# This script is the entry point script that is run inside of the Docker image
# for running the experiment for a single project

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Number of rounds"
    exit
fi

slug=$1
rounds=$2

date

# Update all tooling
su - awshi2 -c "cd /home/awshi2/dt-fixing-tools/; git pull"
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/update.sh"

# Start the run_project.sh script using the awshi2 user
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/run_project.sh ${slug} ${rounds}"

# Change permissions of results and copy outside the Docker image (assume outside mounted under /Scratch)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
cp -r /home/awshi2/output/ /Scratch/${modifiedslug}_output/
chown -R $(id -u):$(id -g) /Scratch/${modifiedslug}_output/
chmod -R 777 /Scratch/${modifiedslug}_output/
