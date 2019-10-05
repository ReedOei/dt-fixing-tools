#!/bin/bash

# This script is the entry point script that is run inside of the Docker image
# for running the experiment for a single project

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] [[ $4 ==  "" ]] || [[ $5 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Number of rounds"
    echo "arg3 - Timeout in seconds"
    echo "arg4 - Full sha of the initial project"
    echo "arg5 - CSV input line for this run"
    echo "arg6 - Test name (Optional)"
    echo "arg7 - Script to run (Optional)"
    echo "arg8 - Script arguments (Optional)"
    exit
fi

slug=$1
rounds=$2
timeout=$3
sha=$4
line=$5
testName=$6
short_sha=${sha:0:7}

# If it's an absolute path, just use it
if [[ "$7" =~ "$/" ]]; then
    script_to_run="$7"
elif [[ -z "$7" ]]; then
    # The default is run_project.sh
    script_to_run="/home/awshi2/dt-fixing-tools/scripts/docker/run_project.sh"
else
    # otherwise, assume it's relative to the docker directory
    script_to_run="/home/awshi2/dt-fixing-tools/scripts/docker/$7"
    script_args="$8"
fi

git rev-parse HEAD
date

sudo apt-get install -y zip unzip
sudo apt-get install -y  ant

# Update all tooling
su - awshi2 -c "cd /home/awshi2/dt-fixing-tools/; git pull"

echo "*******************REED************************"
echo "Running update.sh"
date
su - awshi2 -c "/home/awshi2/dt-fixing-tools/scripts/docker/update.sh"

# Copy the test time log, if it is in the old location. Probably can remove this line if all containers are new.

if [[ -e "/home/awshi2/mvn-test-time.log" ]] && [[ ! -e "/home/awshi2/$slug/mvn-test-time.log" ]]; then
    cp "/home/awshi2/mvn-test-time.log" "/home/awshi2/$slug"
fi

# Start the script using the awshi2 user
su - awshi2 -c "$script_to_run ${slug} ${rounds} ${timeout} ${testName} ${script_args}"

# Change permissions of results and copy outside the Docker image (assume outside mounted under /Scratch)
modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
output_dir=/Scratch/${modifiedslug}-${short_sha}=${testName}_output/
cp -r /home/awshi2/output/ $output_dir
echo "$line" > $output_dir/input.csv
chown -R $(id -u):$(id -g) /Scratch/${modifiedslug}=${testName}_output/
chmod -R 777 /Scratch/${modifiedslug}=${testName}_output/
