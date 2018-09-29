#!/bin/bash

if [[ $1 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    exit
fi

projfile=$1

# Create base Docker image if does not exist
docker inspect detectorbase:latest > /dev/null 2>&1
if  [ $?  == 1 ]; then
    docker build -t detectorbase:latest - < baseDockerfile
fi

# For each project,sha, make a Docker image for it
for line in $(cat ${projfile}); do
    # Create the corresponding Dockerfile
    slug=$(echo ${line} | cut -d',' -f1 | rev | cut -d'/' -f1-2 | rev)
    sha=$(echo ${line} | cut -d',' -f2)
    ./create_dockerfile.sh ${slug} ${sha}

    # Build the Docker image if does not exist
    modifiedslug=$(echo ${slug} | sed 's;/;.;')
    image=detector-${modifiedslug}:latest
    docker inspect ${image} > /dev/null 2>&1
    if [ $? == 1 ]; then
        docker build -t ${image} - < ${modifiedslug}_Dockerfile
    fi

    # Run the Docker image
    # TBD
done
