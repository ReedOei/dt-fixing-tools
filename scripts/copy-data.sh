BASE_DIR="$1"
if [[ -z "$BASE_DIR" ]]; then
    BASE_DIR="$(pwd)"
fi

copy() {
    echo "Looking for $1 in $BASE_DIR"

    for fname in $(find "$BASE_DIR" -name "$1"); do
        outname="$(basename $(dirname $fname))-$(basename $fname)"

        echo "Copying $fname to all-results/$outname"
        cp -r "$fname" "all-results/$outname"
    done
}

mkdir -p all-results

copy "pollution-data"
copy "minimized"
copy "detection-results"
copy "static-field-info"

zip -r all-results.zip all-results

