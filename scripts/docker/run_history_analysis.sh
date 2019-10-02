#!/usr/bin/env bash

echo "*******************REED************************"
echo "Starting run_history_analysis.sh"

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
testName=$( echo ${fullTestName} | rev | cut -d . -f 1 | rev )


# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running the history analysis tools"

idflakiesSha=$( git rev-parse HEAD )
echo $idflakiesSha

date

MVNOPTIONS="-Denforcer.skip=true -Drat.skip=true -Dmdep.analyze.skip=true -Dmaven.javadoc.skip=true"

# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}

# Incorporate tooling into the project, using Java XML parsing
/home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .

# Step 1 : Run the entire test suite and save all tests' file
timeout ${timeout}s /home/awshi2/apache-maven/bin/mvn testrunner:testplugin ${MVNOPTIONS} -Dtestplugin.className=edu.illinois.cs.dt.tools.utility.GetTestFilePlugin -fn -B -e |& tee get-test-file.log

cp get-test-file.log ${RESULTSDIR}

# Step 2 : Get file for specific test name
echo "" > /home/awshi2/commits.log

# Redirect to a different name in case the test-to-file.csv is already in .
# This is necessary because cat cannot read and redirect output to the same file
find . -name test-to-file.csv | xargs cat > ./test-to-file-temp.csv
mv -f test-to-file-temp.csv test-to-file.csv

# Output warning if test suite contains multiple tests with same name
numTestName=$(grep "$fullTestName," /home/awshi2/$slug/test-to-file.csv | wc -l | tr -d '[:space:]')
if [[ "$numTestName" -gt "1" ]];
then
  echo "Warning: Multiple tests with same name found. Choosing first one to proceed." >> /home/awshi2/commits.log
  grep "$fullTestName," /home/awshi2/$slug/test-to-file.csv >> /home/awshi2/commits.log
  echo "" >> /home/awshi2/commits.log
fi

testInfo=$(grep "$fullTestName," /home/awshi2/$slug/test-to-file.csv | head -1)
testFile=$(echo $testInfo | cut -d"," -f2)
moduleName=$(echo $testInfo | cut -d"," -f3)

cp /home/awshi2/$slug/test-to-file.csv ${RESULTSDIR}

# Step 3 : Get all commits for specific test file
maxCommits=$(git log --follow $testFile | grep '^commit ' | wc -l | tr -d '[:space:]')
rawCommits=$(git log --follow $testFile | grep '^commit ')
commits=$(echo ${rawCommits//commit/""} | rev)
echo "latest to oldest commits (total $maxCommits):" > /home/awshi2/commits.log
echo $commits | rev >> /home/awshi2/commits.log

# Step 4 : Get an earlier commit
foundFlakyCommit=""
i=1


# loop until we've found the earliest commit that reveals this flaky test
while [[ $foundFlakyCommit == "" ]];
do
  if [[ $i > ($maxCommits + 1) ]];
  then
    echo "At latest commit already. Number of commits including latest: $maxCommits" >> /home/awshi2/commits.log
    break
  elif [[ $i == ($maxCommits + 1) ]];
  then
      longSha=$idflakiesSha
  else
      longSha=$(echo $commits | cut -d" " -f$i | rev)
  fi

  shortSha=${longSha:0:7}

  REVRESULTSDIR=$RESULTSDIR/$shortSha
  mkdir -p ${REVRESULTSDIR}

  # Step 5 : Clone project again with that commit
  cd /home/awshi2/
  git clone /home/awshi2/$slug $slug-$shortSha && cd $slug-$shortSha && git checkout $longSha

  # If not empty, then there exist class file that matches $className and contains $testName
  # foundTest=$(grep -R "$testName" . | cut -d':' -f1 | grep "$className")
  # if [[ $foundTest == "" ]];
  # then
  #   echo "Test not in this revision ($longSha)" >> /home/awshi2/commits.log
  #   ((i=i+1))
  #   cd /home/awshi2
  #   rm -rf /home/awshi2/$slug-$shortSha
  #   continue
  # fi

  timeout 1h /home/awshi2/apache-maven/bin/mvn clean install -DskipTests -fn -B |& tee /home/awshi2/$slug-$shortSha/mvn-test.log

  /home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .

  timeout ${timeout}s /home/awshi2/apache-maven/bin/mvn testrunner:testplugin ${MVNOPTIONS} -Dtestplugin.className=edu.illinois.cs.dt.tools.utility.GetTestFilePlugin -fn -B -e |& tee get-test-file.log

  find . -name test-to-file.csv | xargs cat > ./test-to-file-temp.csv
  mv -f test-to-file-temp.csv test-to-file.csv

  cp get-test-file.log ${REVRESULTSDIR}
  cp test-to-file.csv ${REVRESULTSDIR}

  foundTest=$(grep -R "$className.$testName," test-to-file.csv | wc -l | tr -d '[:space:]')
  moduleNameRev=$(grep "$className.$testName," ./test-to-file.csv | head -1 | cut -d"," -f3)
  if [[ "$foundTest" -gt "1" ]];
  then
    echo "Warning: Multiple tests with same name found in $i / $maxCommits revision ($longSha). Choosing first one to proceed." >> /home/awshi2/commits.log
    grep "$className.$testName," ./test-to-file.csv >> /home/awshi2/commits.log
    echo "" >> /home/awshi2/commits.log
  elif [[ "$foundTest" -eq "0" ]];
  then
    isTestToFileEmpty=$(cat test-to-file.csv)
    if [[ $isTestToFileEmpty != "" ]];
    then
      echo "Test not in $i / $maxCommits revision ($longSha)" >> /home/awshi2/commits.log
    else
      echo "Could not generate test-to-file.csv in $i / $maxCommits revision ($longSha)" >> /home/awshi2/commits.log
    fi

    cp mvn-test.log ${REVRESULTSDIR}

    ((i=i+1))
    cd /home/awshi2
    rm -rf /home/awshi2/$slug-$shortSha
    continue
  fi

  # Not sure if the mvn-test.log and mvn-test-time.log is even needed anymore. if not can skip the next line
  { time -p timeout 1h /home/awshi2/apache-maven/bin/mvn test  -fn -B |& tee -a /home/awshi2/$slug-$shortSha/mvn-test.log ;} 2> /home/awshi2/$slug-$shortSha/mvn-test-time.log

  # Step 6 : Run iDFlakies on that commit
  /home/awshi2/dt-fixing-tools/scripts/docker/run_random_class_method.sh $slug-$shortSha ${rounds} ${timeout} ${REVRESULTSDIR} ${moduleNameRev}

  # Step 7 : Check whether iDFlakies found flaky tests
  files=$(find /home/awshi2/$slug-$shortSha/ -name list.txt)
  if [[ $(cat $files | wc -l) = "0" ]]; then
    echo "No DTs found in $i / $maxCommits revision ($longSha)" >> /home/awshi2/commits.log
  else
    foundFlakyCommit=$(grep "$className.$testName$" $files)
    if [[ $foundFlakyCommit != "" ]];
    then
      echo "First commit where test is flaky ($i / $maxCommits):" >> /home/awshi2/commits.log
      echo $longSha >> /home/awshi2/commits.log

      cp mvn-test.log ${REVRESULTSDIR}
      cp mvn-test-time.log ${REVRESULTSDIR}
      break
    else
      echo "DTs were found in $i / $maxCommits revision ($longSha) but not matching $fullTestName" >> /home/awshi2/commits.log
    fi
  fi

  cp mvn-test.log ${REVRESULTSDIR}
  cp mvn-test-time.log ${REVRESULTSDIR}

  cd /home/awshi2
  rm -rf /home/awshi2/$slug-$shortSha
  ((i=i+1))

done

cp /home/awshi2/commits.log ${RESULTSDIR}

echo "*******************REED************************"
echo "Finished run_history_analysis.sh"
date
