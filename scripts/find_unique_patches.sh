#!/bin/bash

if [[ $1 == "" ]] || [[ $2 == "" ]]; then
    echo "arg1 - directory with results of debugging"
    echo "arg2 - file with list of tests, types, and modules (list_od_test_type.csv)"
    exit
fi

debuggingresults=$1
testsfile=$2

overalluniquepatches=0
overalluniquepatchessizes=0
overalluniquepatchescount=0
overallworkingpatches=0
overallworkingpatchescount=0
overallpossiblepatches=0
overallpossiblepatchescount=0

overalluniquepatchespertest=0
overalluniquepatchessizespertest=0
overalltestcount=0

IFS=$'\n'
for module in $(grep -v "#" ${testsfile} | cut -d',' -f2 | sort -u); do
    # Make a temporary file but manipulate it through a file descriptor, so it gets deleted regardless when script ends
    tmpfile=$(mktemp /tmp/find_unique_patches.XXXXXX)
    exec 3> "${tmpfile}"
    rm "${tmpfile}"

    testcount=0
    pairscount=0
    count=0

    rollingpertestunique=0
    rollingpertestuniquesize=0

    for t in $(grep ",${module}" ${testsfile} | grep -v "#" | cut -d',' -f1); do
        d=$(find ${debuggingresults} -maxdepth 4 -name fixer.log | grep "=${t}/" | xargs -n1 dirname)
        # Count how many fixed dependent tests there are
        if [[ $(grep -r "INLINE" ${d}) != "" ]]; then
            testcount=$((testcount + 1))
        fi

        # Make a temporary file but manipulate it through a file descriptor, so it gets deleted regardless when script ends
        tmpfile=$(mktemp /tmp/find_unique_patches.XXXXXX)
        exec 4> "${tmpfile}"
        rm "${tmpfile}"

        # Count unique patches
        for p in $(find ${d} -name "*.patch*"); do
            if [[ $(grep "NO CLEANERS" ${p}) == "" ]]; then
                pairscount=$((pairscount + 1))
            fi
            if [[ $(grep "INLINE" ${p}) != "" ]] || [[ $(grep "CLEANER DOES NOT FIX" ${p}) != "" ]] || [[ $(grep "BROKEN MINIMAL" ${p}) != "" ]]; then
                size=$(grep "NEW CLEANER SIZE: " ${p} | cut -d':' -f2 | xargs)
                if [[ ${size} == "N/A" ]]; then
                    continue
                fi
                count=$((count + 1))
                linenum=$(grep -n "============" ${p} | cut -d':' -f1)
                modified=$(grep -h "MODIFIED" ${p})
                # Checking both patch contents and the name of the modified method
                #echo $(md5sum <(echo "$(echo ${modified}) $(tail -n +$((linenum + 1)) ${p})") | cut -d' ' -f1) ${size} >>3
                echo $(md5sum <(echo "$(tail -n +$((linenum + 1)) ${p})") | cut -d' ' -f1) ${size} >>3

                # Count up per test
                echo $(md5sum <(echo "$(tail -n +$((linenum + 1)) ${p})") | cut -d' ' -f1) ${size} >>4
            fi
        done
        rollingpertestunique=$(echo "$(sort -u <4 | wc -l) + ${rollingpertestunique}" | bc -l)
        rollingpertestuniquesize=$(echo "$(sort -u <4 | cut -d' ' -f2 | sort -u | wc -l) + ${rollingpertestuniquesize}" | bc -l)
        echo -n "" >4
    done

    echo "\\Def{${module}_fixed_tests}{${testcount}}"

    # Unique patches across every test in the module
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_unique_patches}{n/a}"
        echo "\\Def{${module}_unique_patches_sizes}{n/a}"
        echo "\\Def{${module}_unique_patches_percentage}{n/a}"
    else
        echo "\\Def{${module}_unique_patches}{$(sort -u <3 | wc -l)}"
        echo "\\Def{${module}_unique_patches_sizes}{$(sort -u <3 | cut -d' ' -f2 | sort -u | wc -l)}"
        echo "\\Def{${module}_unique_patches_percentage}{$(echo "$(sort -u <3 | wc -l) / ${count}" | bc -l | xargs printf "%.1f")\\%}"
        overalluniquepatches=$(echo "$(sort -u <3 | wc -l) + ${overalluniquepatches}" | bc -l)
        overalluniquepatchessizes=$(echo "$(sort -u <3 | cut -d' ' -f2 | sort -u | wc -l) + ${overalluniquepatchessizes}" | bc -l)
        overalluniquepatchescount=$((overalluniquepatchescount + 1))
    fi

    # Working patches across every test in the module
    if [[ ${count} == 0 ]]; then
        echo "\\Def{${module}_working_patches}{n/a}"
    else
        echo "\\Def{${module}_working_patches}{${count}}"
        overallworkingpatches=$(echo "${overallworkingpatches} + ${count}" | bc -l)
        overallworkingpatchescount=$((overallworkingpatchescount + 1))
    fi

    # Possible patches across every test in the module (should be the same as working...)
    if [[ ${pairscount} == 0 ]]; then
        echo "\\Def{${module}_possible_patches}{n/a}"
    else
        echo "\\Def{${module}_possible_patches}{${pairscount}}"
        overallpossiblepatches=$(echo "${overallpossiblepatches} + ${pairscount}" | bc -l)
        overallpossiblepatchescount=$((overallpossiblepatchescount + 1))
    fi

    # Unique patches per test, averaged across tests in this module
    if [[ ${testcount} == 0 ]]; then
        echo "\\Def{${module}_unique_patches_pertest}{n/a}"
        echo "\\Def{${module}_unique_patches_sizes_pertest}{n/a}"
    else
        avgunique=$(echo "${rollingpertestunique} / ${testcount}" | bc -l)
        echo "\\Def{${module}_unique_patches_pertest}{$(echo "${avgunique}" | xargs printf "%.1f")}"
        avgsizeunique=$(echo "${rollingpertestuniquesize} / ${testcount}" | bc -l)
        echo "\\Def{${module}_unique_patches_sizes_pertest}{$(echo "${avgsizeunique}" | xargs printf "%.1f")}"
        overalluniquepatchespertest=$(echo "${rollingpertestunique} + ${overalluniquepatchespertest}" | bc -l)
        overalluniquepatchessizespertest=$(echo "${rollingpertestuniquesize} + ${overalluniquepatchessizespertest}" | bc -l)
        overalltestcount=$(echo "${overalltestcount} + ${testcount}" | bc -l)
    fi
    echo -n "" >3
done

# Output the overall macros
echo "\\Def{average_unique_patches}{$(echo ${overalluniquepatches} / ${overalluniquepatchescount} | bc -l | xargs printf "%.1f")}"
echo "\\Def{average_unique_patches_sizes}{$(echo ${overalluniquepatchessizes} / ${overalluniquepatchescount} | bc -l | xargs printf "%.1f")}"
echo "\\Def{average_working_patches}{$(echo ${overallworkingpatches} / ${overallworkingpatchescount} | bc -l | xargs printf "%.1f")}"
echo "\\Def{average_possible_patches}{$(echo ${overallpossiblepatches} / ${overallpossiblepatchescount} | bc -l | xargs printf "%.1f")}"

echo "\\Def{total_testcount}{${overalltestcount}}"
echo "\\Def{average_unique_patches_pertest}{$(echo ${overalluniquepatchespertest} / ${overalltestcount} | bc -l | xargs printf "%.1f")}"
echo "\\Def{average_unique_patches_sizes_pertest}{$(echo ${overalluniquepatchessizespertest} / ${overalltestcount} | bc -l | xargs printf "%.1f")}"

echo "\\Def{total_unique_patches}{${overalluniquepatches}}"
echo "\\Def{total_working_patches}{${overallworkingpatches}}"
