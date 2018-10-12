#!/bin/bash

# This script is the entry point script that is run inside of the Docker image
# for running the experiment for a single project

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Timeout in seconds"
    exit
fi

slug=$1
timeout=$2

git rev-parse HEAD
date

# Update all tooling
su - awshi2 -c "cd /home/awshi2/dt-fixing-tools/; git pull"

echo "*******************REED************************"
echo "Running update.sh"
date
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/update.sh"

# Start the run_project.sh script using the awshi2 user
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/run_project.sh ${slug} ${timeout}"

# Change permissions of results and copy outside the Docker image (assume outside mounted under /Scratch)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
cp -r /home/awshi2/output/ /Scratch/${modifiedslug}_output/
chown -R $(id -u):$(id -g) /Scratch/${modifiedslug}_output/
chmod -R 777 /Scratch/${modifiedslug}_output/
