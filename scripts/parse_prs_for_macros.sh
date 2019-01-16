#!/bin/bash

prscsv=$1

if [[ ${prscsv} == "" ]]; then
    echo "arg - csv with info on PRs"
    exit
fi

echo "\\Def{NumProjWithPRSubmitted}{$(grep ",Y," "${prscsv}" | cut -d',' -f1 | sort -u | wc -l)}"
echo "\\Def{NumProjWithPRAccepted}{$(grep ",Y,Y" "${prscsv}" | cut -d',' -f1 | sort -u | wc -l)}"
echo "\\Def{NumTestsWithPRSubmitted}{$(grep ",Y," "${prscsv}" | sort -u | wc -l)}"
echo "\\Def{NumTestsWithPRAccepted}{$(grep ",Y,Y" "${prscsv}" | sort -u | wc -l)}"

echo "\\Def{NumTestsAlreadyFixed}{$(grep ",N,N,Y,N" "${prscsv}" | sort -u | wc -l)}"
echo "\\Def{NumTestsNoFix}{$(grep ",N,N,N,Y" "${prscsv}" | sort -u | wc -l)}"
