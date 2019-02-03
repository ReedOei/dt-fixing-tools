#!/usr/bin/env bash

# Usage: bash update-fix-method-order.sh DATABASE

if [[ $1 == "" ]]; then
    echo "arg1 - Path to database to update"
    exit
fi

database="$1"

echo "[INFO] Updating FixMethodOrder info"
# QUERY="select s.slug, ftc.subject_name, ftc.test_name from fixable_tests ftc join subject s on ftc.subject_name = s.name;"
QUERY="select s.slug, ftc.subject_name, ftc.test_name, sr.sha from fixable_tests ftc join subject s on ftc.subject_name = s.name join subject_raw sr on sr.slug = s.slug;"
lines=$(echo "$QUERY" | sqlite3 "$database")
for line in $lines; do
    slug=$(echo $line | cut -d'|' -f1)
    subject_name=$(echo $line | cut -d'|' -f2)
    test_name=$(echo $line | cut -d'|' -f3)
    sha=$(echo $line | cut -d'|' -f4)
    # Get everything before the last '.'
    test_class_name=$(echo $test_name | rev | cut -f2- -d"." | rev)

    if [[ ! -d "temp-subject/$slug" ]]; then
        mkdir -p "temp-subject"
        git clone "https://github.com/$slug" "temp-subject/$slug"
        (
            cd "temp-subject/$slug"
            git checkout $sha
        )
    fi

    if find "temp-subject/$slug" -name "*.java" | grep -E "$test_class_name.java$" | xargs cat | grep -q "@FixMethodOrder"; then
        echo "@FixMethodOrder found: $test_name"
        echo "update original_order set fix_method_order = 1 where test_name = '$test_name' and subject_name='$subject_name';" | sqlite3 "$database"
    else
        echo "@FixMethodOrder NOT found: $test_name"
        echo "update original_order set fix_method_order = 0 where test_name = '$test_name' and subject_name='$subject_name';" | sqlite3 "$database"
    fi
done

