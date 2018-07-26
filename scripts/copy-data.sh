copy() {
    for fname in $(find -name "$1"); do
        outname="$(basename $(dirname $fname))-$(basename $fname)"

        echo "Copying $fname to all-results/$outname"
        cp -r "$fname" "all-results/$outname"
    done
}

rm -rf all-results
mkdir all-results

copy "pollution-data"
copy "minimized"
copy "detection-results"
copy "static-field-info"

zip -r all-results.zip all-results

