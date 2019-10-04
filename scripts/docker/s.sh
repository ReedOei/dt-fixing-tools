outputFile=$1
testName=$2
className=$3
idflakiesSha=$4

echo "" > $outputFile

for first in $(git log -G "${testName}" --pretty=%H  | tac); do
  file_names=$(git show ${first} | egrep "^\\+\+\+|${testName}" | grep -B1 "${testName}" | egrep "^\+\+\+" | rev | cut -b6- | cut -f1 -d/ | rev)

  echo "Suspected first commit is $first" >> $outputFile

  if [[ $file_names =~ $className ]];
  then
    echo "First commit does contain likely test file." >> $outputFile
    count=$(($(git rev-list --count $first..$idflakiesSha) - 1))
    echo "Number of commits between first ($first) and iDFlakies ($idflakiesSha) commit: $count" >> $outputFile
    break
  else
    echo "First commit does not contain likely test file." >> $outputFile
    echo "Expected className is $className, while file names found are:" >> $outputFile
    echo $file_names >> $outputFile
    # git show ${first} >> /home/awshi2/commits.log
  fi
done
