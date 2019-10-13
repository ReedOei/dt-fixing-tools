outputFile=$1
testName=$2
testNameWithParen="$testName\s*\("
className=$3
idflakiesSha=$4

echo "testName: $testName" > $outputFile
echo "className: $className" >> $outputFile
echo "idflakiesSha: $idflakiesSha" >> $outputFile
echo "commits to consider are: $(git log -G "${testNameWithParen}" --pretty=%H  | tac)" >> $outputFile

for first in $(git log -G "${testNameWithParen}" --pretty=%H  | tac); do
  file_names=$(git show ${first} | egrep "^\\+\+\+|${testName}" | grep -B1 "${testName}" | egrep "^\+\+\+" | rev | cut -b6- | cut -f1 -d/ | rev)

  echo "Suspected first commit is $first" >> $outputFile

  firstShaTime=$(git log $first -n1 --date=raw --format="%ad" |cut -d' ' -f1)
  iDShaTime=$(git log $idflakiesSha -n1 --date=raw --format="%ad" |cut -d' ' -f1)
  timeDiff=$(( iDShaTime - firstShaTime ))
  if [[ timeDiff -lt 0 ]]; then
      timeDiff="NEGATIVE"
  fi
  echo "Time between first ($first) and iDFlakies ($idflakiesSha) commit (s): $timeDiff" >> $outputFile

  
  firstChangedFiles=$(git show $first --numstat --format=oneline | sed '1d' | wc -l)
  firstAddedLines=0
  for n in $(git show $first --numstat --format=oneline | sed '1d' | cut -d' ' -f1); do ((firstAddedLines += $n)); done
  firstRemovedLines=0
  for n in $(git show $first --numstat --format=oneline | sed '1d' | cut -d' ' -f2); do ((firstRemovedLines += $n)); done
  echo "Files changed in first ($first) commit: $firstChangeFiles" >> $outputFile
  echo "Lines added in first ($first) commit: $firstAddedLines" >> $outputFile
  echo "Lines removed in first ($first) commit: $firstRemovedLines" >> $outputFile
  
  if [[ $file_names =~ $className ]];
  then
    echo "First commit does contain likely test file." >> $outputFile
    count=$(($(git rev-list --ancestry-path --count $first..$idflakiesSha) - 1))
    echo "Number of commits between first ($first) and iDFlakies ($idflakiesSha) commit: $count" >> $outputFile
    break
  else
    echo "First commit does not contain likely test file." >> $outputFile
    echo "Expected className is $className, while file names found are:" >> $outputFile
    echo $file_names >> $outputFile
    # git show ${first} >> /home/awshi2/commits.log
  fi
  
done
