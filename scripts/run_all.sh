subjNum="1"
maxSubjNum="13"
timesToRun="100"
dataDir="data/all_split"


while [ $subjNum -le $maxSubjNum ]
do
  dirPath="docker/"
  # dirPath="docker_$subjNum/"
  # if [ ! -d "$dirPath" ]; then
  #   echo "$dirPath does not exist. Creating now..."
  #   cp -r docker/$dirPath
  # fi

  dataFile="$dataDir/sorted_subject_data_$subjNum.csv"
  if [ ! -f "$dataFile" ]; then
    echo "$dataFile does not exist. Skipping..."
    continue
  fi

  cd $dirPath
  nohup bash create_and_run_dockers.sh $dataFile $timesToRun > $subjNum.out 2>&1 &
  cd ../
  
  subjNum=$[$subjNum+1]
done
