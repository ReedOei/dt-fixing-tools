#!/bin/bash

# This script is the entry point script that is run inside of the Docker image
# for running the experiment for a single project

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Number of rounds"
    echo "arg3 - Timeout in seconds"
    echo "arg4 - Script to run (Optional)"
    exit
fi

if [[ "$4" =~ "$/" ]]; then
    script_to_run="$4"
elif [[ -z "$4" ]]; then
    script_to_run="/home/awshi2/dt-fixing-tools/script/docker/run_project.sh"
else
    script_to_run="/home/awshi2/dt-fixing-tools/script/docker/$4"
fi

slug=$1
rounds=$2
timeout=$3

git rev-parse HEAD
date

# Update all tooling
su - awshi2 -c "cd /home/awshi2/dt-fixing-tools/; git pull"

echo "*******************REED************************"
echo "Running update.sh"
date
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/update.sh"

# Start the run_project.sh script using the awshi2 user
su - awshi2 -c "$script_to_run ${slug} ${rounds} ${timeout}"

# Change permissions of results and copy outside the Docker image (assume outside mounted under /Scratch)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
cp -r /home/awshi2/output/ /Scratch/${modifiedslug}_output/
chown -R $(id -u):$(id -g) /Scratch/${modifiedslug}_output/
chmod -R 777 /Scratch/${modifiedslug}_output/
