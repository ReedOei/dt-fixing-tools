#!/usr/bin/env bash

# Usage: bash build-database.sh RESULTS_FOLDER DATABASE SUBJECT_LIST

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]]; then
    echo "arg1 - Results folder"
    echo "arg2 - Path to database to create"
    echo "arg3 - List of subjects/shas in CSV (format: url,sha)"
    exit
fi

results_folder="$1"
database="$2"
subject_list="$3"

scripts_folder=$(cd "$(dirname $BASH_SOURCE)"; pwd)

if [[ ! "$results_folder" =~ "$/" ]]; then
    results_folder="$(cd $results_folder; pwd)"
fi

if [[ ! "$database" =~ "$/" ]]; then
    database="$(cd "$(dirname $database)"; pwd)/$(basename $database)"
fi

if [[ ! "$subject_list" =~ "$/" ]]; then
    subject_list="$(cd "$(dirname $subject_list)"; pwd)/$(basename $subject_list)"
fi

# Go to where the pom is
cd "$scripts_folder/.."

subject_list_loc="$(pwd)/subject_loc.csv"
> $subject_list_loc # Clear the file

# download and count the lines
# for line in $(cat "$subject_list"); do
#     slug=$(echo ${line} | cut -d',' -f1 | rev | cut -d'/' -f1-2 | rev)
#     sha=$(echo $line | cut -f2 -d",")

#     mkdir -p "temp-subject"
#     download_path="temp-subject/$slug"
#     echo "Counting lines for $slug"
#     git clone "https://github.com/$slug" "temp-subject/$slug" &> /dev/null

#     (
#         cd "$download_path"
#         git checkout "$sha"
#     ) &> /dev/null

#     c=$(cloc "$download_path" --include-lang=Java --csv --quiet | tail -1)
#     tc=$(cloc "$download_path" --match-f=".*Test.*" --include-lang=Java --csv --quiet | tail -1)

#     loc=$(echo $c | cut -f5 -d",")
#     test_loc=$(echo $tc | cut -f5 -d",")

#     echo "$slug,$sha,$loc,$test_loc" |& tee -a "$subject_list_loc"

#     # rm -rf "$download_path"
# done

# rm -rf temp-subject

mvn install -DskipTests exec:java \
    -Dexec.mainClass="edu.illinois.cs.dt.tools.analysis.Analysis" \
    -Dexec.args="--results '$results_folder' --db '$database' --subjectList '$subject_list' --subjectListLoc '$subject_list_loc'"

