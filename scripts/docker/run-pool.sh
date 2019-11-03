#!/usr/bin/env bash

if [[ $1 == "" ]] || [[ $2 == "" ]] || [[ $3 == "" ]]; then
    echo "arg1 - Path to CSV file with project,sha"
    echo "arg2 - Number of rounds"
    echo "arg3 - Timeout in seconds"
    echo "arg4 - The script to run (Optional)"
    echo "arg5 - Number of processes to run at the same time (Optional)"
    echo "arg6 - Script arguments (Optional)"
    echo "arg7 - Time to wait before starting each process (Optional)"
    exit
fi

date

PROCESS_NUM="$5"
WAIT_TIME=$7

if [[ -z "$WAIT_TIME" ]]; then
    WAIT_TIME=0
fi


if [[ -z "$PROCESS_NUM" ]]; then
    PROCESS_NUM="1"
fi

#find "$1" -maxdepth 1 -type f -name "*.csv" | xargs -P"$PROCESS_NUM" -I{} bash run-project-pool.sh {} "$2" "$3" "$4" "$6"
# blah="";i=0; for f in $(find data/run_10m_22d_missing_reverse_first_sha_exp/ -maxdepth 1 -type f -name "*.csv"); do blah="$blah $f,$i"; ((i+=5)); done; date; echo "$blah" | xargs -I% -P2 -d' ' sh -c '{ if [ ! -z "%" ]; then csv=$(echo "%" | cut -d, -f1 ); waittime=$(echo "%" | cut -d, -f2 ); sleep $waittime ; bash test.sh $csv "$(date)" ; fi; }''
input_str=""
i=0
processes=0

for f in $(find "$1" -maxdepth 1 -type f -name "*.csv"); do
    if [ "$PROCESS_NUM" -gt "$processes" ]; then
        input_str="$input_str $f,$i"
        ((i+=$WAIT_TIME))
        ((processes+=1))
    else
        input_str="$input_str $f,0"
    fi
done

#echo "$input_str" | xargs -I% -P"$PROCESS_NUM" -d' ' bash -c 'if [ ! -z "%" ]; then csv=$(echo "%" | cut -d, -f1 ); waittime=$(echo "%" | cut -d, -f2 ); sleep $waittime ; bash test.sh $csv '"$2"' ; fi;'

echo "$input_str" | xargs -I% -P"$PROCESS_NUM" -d' ' sh -c '{ if [ ! -z "%" ]; then csv=$(echo "%" | cut -d, -f1 ); waittime=$(echo "%" | cut -d, -f2 ); sleep $waittime ; bash run-project-pool.sh $csv '"$2"' '"$3"' '"$4"' '"$6"' ; fi; }'

date
