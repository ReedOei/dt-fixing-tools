#!/bin/bash

# This script is the entry point script that is run inside of the Docker image
# for running the experiment for a single project

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] || [[ $4 ==  "" ]] || [[ $5 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Full sha of the initial project"
    echo "arg3 - CSV input line for this run"
    echo "arg4 - Script to run"
    echo "arg5 - Script arguments"
    exit
fi

slug=$1
sha=$2
line=$3
script_to_run=$4
shift 4
short_sha=${sha:0:7}

git rev-parse HEAD
date

# Update all tooling
su - awshi2 -c "cd /home/awshi2/dt-fixing-tools/; git pull"
su - awshi2 -c "cd /home/awshi2/dt-fixing-tools/; /home/awshi2/apache-maven/bin/mvn clean install"

# Start the script using the awshi2 user
su - awshi2 -- ${script_to_run} ${slug} "$@" 

# Change permissions of results and copy outside the Docker image (assume outside mounted under /Scratch)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')

folder_name=${modifiedslug}-${short_sha}=_output

output_dir=/Scratch/$folder_name
echo "$line" > /home/awshi2/output/input.csv

cp -r /home/awshi2/output/ $output_dir
chown -R $(id -u):$(id -g) $output_dir
chmod -R 777 $output_dir
