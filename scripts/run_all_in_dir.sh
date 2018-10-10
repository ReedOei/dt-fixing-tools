timesToRun="2"
dataDir="data/test_inputs"
dirPath="docker"

for d in $dataDir/*.csv/ ; do
  dataFile=$d
  dockerDataFile="$dirPath/$dataFile"
  if [ ! -f "$dockerDataFile" ]; then
    echo "$dockerDataFile does not exist. Skipping..."
    subjNum=$[$subjNum+1]
    continue
  fi

  filename=$(basename -- "$dataFile")
  
  cd $dirPath
  nohup bash create_and_run_dockers.sh $dataFile $timesToRun > $filename.out 2>&1 &
  cd ../
done
