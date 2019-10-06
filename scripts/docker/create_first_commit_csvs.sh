# Usage: bash create_first_commit_csvs.sh data/first-sha/allflakies-all-first-sha data/first-sha/test-to-first-sha.csv data/allflakies-all

# test-first-sha.csv are generated with: for f in $(find -name "commits.log" | xargs grep "Number of commits between first" | sort); do echo "$(echo $f | cut -d'=' -f 2 | cut -d':' -f 1 | rev | cut -d'_' -f 2- | rev),$( echo $f | cut -d'(' -f 2 | cut -d')' -f 1),$( echo $f | rev | cut -d':' -f 1 | rev | xargs)"; done > test-to-first-sha.csv

output_dir=$1
test_to_first_sha_file=$2
input_dir=$3

mkdir -p $output_dir
for line in $(cat $test_to_first_sha_file); do
    test_name=$(echo $line | cut -d, -f 1)
    new_sha=$(echo $line | cut -d, -f 2)
    short_sha=${new_sha:0:7}

    file_path=$(grep ",$test_name," $input_dir/* -l | tail -n 1)
    if [[ $file_path == "" ]];
    then
        continue
    fi


    file_content=$(cat $file_path)
    url=$(cat $file_path | cut -d, -f 1)
    rest=$(cat $file_path | cut -d, -f 3-)

    new_content="$url,$new_sha,$rest"

    old_file_name=$(echo $file_path | rev | cut -d/ -f 1 | rev)
    proj_name=$(echo $old_file_name | cut -d'=' -f 1 | rev | cut -d'-' -f 2- | rev)
    rest_name=$(echo $old_file_name | cut -d'=' -f 2-)

    file_name=$proj_name-$short_sha-$rest_name

    new_file_path="$output_dir/$file_name"
    echo $new_content > $new_file_path
    echo "Created file: $new_file_path"
done
