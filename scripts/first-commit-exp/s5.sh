for d in */*; do
    ( cd $d
      { echo [; find -name "round*.json" | grep "random/"  | xargs -n 1 -I{} cat {} ../../, ; echo \"\" ];  } | prettyjson | grep "\"name\"" > $d/names.txt
    )
done
