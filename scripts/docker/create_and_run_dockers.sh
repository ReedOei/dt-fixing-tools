#!/bin/bash

if [[ $1 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    exit
fi

projfile=$1

# Create base docker image
docker build -t detectorbase:latest - < baseDockerfile

# For each project,sha, make a docker image for it
for line in $(cat ${projfile}); do
    # Create the corresponding docker file
    slug=$(echo ${line} | cut -d',' -f1)
    sha=$(echo ${line} | cut -d',' -f2)
    ./create_dockerfile.sh ${slug} ${sha}

    # Build the docker image
    modifiedslug=$(echo ${slug} | sed 's;/;.;')
    image=detector-${modifiedslug}:latest
    docker build -t ${image} - < ${modifiedslug}_Dockerfile

    # Run the docker image
done
