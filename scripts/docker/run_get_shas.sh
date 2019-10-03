#!/usr/bin/env bash

echo "*******************REED************************"
echo "Starting run_get_shas.sh"

date

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]] || [[ $4 == "" ]]; then
  echo "arg1 - GitHub SLUG"
  echo "arg2 - Number of rounds"
  echo "arg3 - Timeout in seconds"
  echo "arg4 - Test name"
  exit
fi

slug=$1
rounds=$2
timeout=$3
fullTestName=$4
fullClassName=$( echo ${fullTestName} | rev | cut -d . -f 2- | rev )
className=$( echo ${fullClassName} | rev | cut -d . -f 1 | rev )
testName="$( echo ${fullTestName} | rev | cut -d . -f 1 | rev ) *( *)"


# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running the get shas tools"

idflakiesSha=$( git rev-parse HEAD )
echo $idflakiesSha

date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}


first=$(git log -G "${testName}" --pretty=%H  | tail -1)
file_name=$(git show ${first} | egrep "^\\+\+\+|${testName}" | grep -B1 "${testName}" | head -1 | rev | cut -b6- | cut -f1 -d/ | rev)

echo "Suspected first commit is $first" > /home/awshi2/commits.log

if [[ $file_name == $className ]]; 
then
  echo "First commit does contain likely test file." >> /home/awshi2/commits.log
  count=$(($(git rev-list --count $first..$idflakiesSha) - 1))
  echo "Number of commits between first ($first) and iDFlakies ($idflakiesSha) commit: $count" >> /home/awshi2/commits.log
else
  echo "First commit does not contain likely test file." >> /home/awshi2/commits.log
  echo "File name found is $file_name while className is $className" >> /home/awshi2/commits.log
  git show ${first} >> /home/awshi2/commits.log
fi

cp /home/awshi2/commits.log ${RESULTSDIR}

echo "*******************REED************************"
echo "Finished run_get_shas.sh"
date