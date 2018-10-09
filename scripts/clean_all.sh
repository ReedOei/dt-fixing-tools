subjNum="1"
maxSubjNum="13"
dataDir="data/all_split"

while [ $subjNum -le $maxSubjNum ]
do
  # dirPath="docker_$subjNum/"
  # if [ ! -d "$dirPath" ]; then
  #   echo "$dirPath does not exist. Skipping..."
  #   continue
  # fi

  dataFile="$dataDir/sorted_subject_data_$subjNum.csv"
  if [ ! -f "$dataFile" ]; then
    echo "$dataFile does not exist. Skipping..."
    continue
  fi

  dirPath="docker/"
  cd $dirPath
  rm -rf *_output/
  rm -rf *.out
  #./cleanimages.sh $dataFile
  cd ../
  
  subjNum=$[$subjNum+1]
done
