#!/bin/bash

git rev-parse HEAD

date

# This script is run inside the Docker image, for single experiment (one project)
# Should only be invoked by the run_experiment.sh script

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]]; then
    echo "arg1 - GitHub SLUG"
    echo "arg2 - Number of rounds"
    echo "arg3 - Timeout in seconds"
    exit
fi

slug=$1
rounds=$2
timeout=$3

# Setup prolog stuff
cd /home/awshi2/dt-fixing-tools/scripts/
./setup

# Set environment up, just in case
source ~/.bashrc

# Incorporate tooling into the project, using Java XML parsing
cd /home/awshi2/${slug}
/home/awshi2/dt-fixing-tools/scripts/docker/pom-modify/modify-project.sh .

# Run the plugin, get module test times
echo "*******************REED************************"
echo "Running testplugin for getting module test time"
date

# Optional timeout... In practice our tools really shouldn't need 1hr to parse a project's surefire reports. 
timeout 1h /home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Dtestplugin.className=edu.illinois.cs.dt.tools.utility.ModuleTestTimePlugin -fn -B -e |& tee module_test_time.log


# Run the plugin, reversing the original order (reverse class and methods)
echo "*******************REED************************"
echo "Running testplugin for reversing the original order"
date

/home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddetector.timeout=3600 -Ddt.randomize.rounds=${rounds} -Ddetector.detector_type=reverse -fn -B -e |& tee reverse_original.log


# Run the plugin, reversing the original order (reverse class)
echo "*******************REED************************"
echo "Running testplugin for reversing the class order"
date

/home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddetector.timeout=3600 -Ddt.randomize.rounds=${rounds} -Ddetector.detector_type=reverse-class -fn -B -e |& tee reverse_class.log


# Run the plugin, original order
echo "*******************REED************************"
echo "Running testplugin for original"
date

/home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddetector.timeout=${timeout} -Ddt.randomize.rounds=${rounds} -Ddetector.detector_type=flaky -fn -B -e |& tee original.log


# Run the plugin, random class first, method second
echo "*******************REED************************"
echo "Running testplugin for randomizemethods"
date

/home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddetector.timeout=${timeout} -Ddt.randomize.rounds=${rounds} -fn -B -e |& tee random_class_method.log


# Run the plugin, random class only
echo "*******************REED************************"
echo "Running testplugin for randomizeclasses"
date

/home/awshi2/apache-maven/bin/mvn testrunner:testplugin -Denforcer.skip=true -Drat.skip=true -Ddetector.timeout=${timeout} -Ddt.randomize.rounds=${rounds} -Ddetector.detector_type=random-class -fn -B -e |& tee random_class.log


# Gather the results, put them up top
RESULTSDIR=/home/awshi2/output/
mkdir -p ${RESULTSDIR}
/home/awshi2/dt-fixing-tools/scripts/gather-results $(pwd) ${RESULTSDIR}
mv module_test_time.log ${RESULTSDIR}
mv original.log ${RESULTSDIR}
mv random_class_method.log ${RESULTSDIR}
mv random_class.log ${RESULTSDIR}
mv reverse_original.log ${RESULTSDIR}
mv reverse_class.log ${RESULTSDIR}


echo "*******************REED************************"
echo "Finished run_project.sh"
date

