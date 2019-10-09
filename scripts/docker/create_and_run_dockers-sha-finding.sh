#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - Path to CSV file with URL,firstSha,flakySha"
    echo "arg2 - Number of working Shas to find"
    exit
fi

git rev-parse HEAD
date

projfile=$1
numberShas=$2

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

echo "*******************REED************************"
echo "Making base image"
date

# Create base Docker image if does not exist
docker inspect detectorbase:latest > /dev/null 2>&1
if  [ $?  == 1 ]; then
    docker build -t detectorbase:latest - < baseDockerfile
fi

echo "*******************REED************************"
echo "Making tooling image"
date

# Create tooling Docker image if does not exist
docker inspect toolingdetectorbase:latest > /dev/null 2>&1
if  [ $?  == 1 ]; then
    docker build -t toolingdetectorbase:latest - < toolingDockerfile
fi

# For each project,sha, make a Docker image for it
for line in $(cat ${projfile}); do
    # Create the corresponding Dockerfile
    slug=$(echo ${line} | cut -d',' -f1 | rev | cut -d'/' -f1-2 | rev)
    firstSha=$(echo ${line} | cut -d',' -f2)
    flakySha=$(echo ${line} | cut -d',' -f3)

    # Build the Docker image if does not exist
    modifiedslug=$(echo ${slug} | sed 's;/;.;' | tr '[:upper:]' '[:lower:]')
    short_sha=${flakySha:0:7}
    modifiedslug_with_sha="${modifiedslug}-${short_sha}"
    ./create_dockerfile.sh ${slug} ${flakySha} ${modifiedslug_with_sha}

    image=detector-${modifiedslug_with_sha}:latest
    docker inspect ${image} > /dev/null 2>&1
    if [ $? == 1 ]; then
        echo "*******************REED************************"
        echo "Building docker image for project"
        date
        bash build_docker_image.sh ${image} ${modifiedslug_with_sha}
    fi

    # Run the Docker image if it exists
    docker inspect ${image} > /dev/null 2>&1
    if [ $? == 1 ]; then
        echo "${image} NOT BUILT PROPERLY, LIKELY TESTS FAILED"
    else
        docker run -t --rm -v ${SCRIPT_DIR}:/Scratch ${image} /bin/bash -x /Scratch/run_find_shas.sh ${slug} ${flakySha} ${line} /Scratch/find_working_shas.sh ${firstSha} ${flakySha} ${numberShas}
     fi
done

