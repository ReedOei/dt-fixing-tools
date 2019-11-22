find -name "*_output-log.txt" |
    egrep -A 1 "(Starting|Finished) run_get_files_tests.sh" doanduyhai.achilles-ac85374\=info.archinnov.achilles.it.TestEntityWithSASIIndices.should_search_using_like_non_tokenizer_output-log.txt | tr -d '\r' | { readarray a ; echo $(( ( $(date +%s -d "$(echo ${a[4]} | tr -d '\n')") - $(date +%s -d "$(echo ${a[1]} | tr -d '\n')") ) / 60 )); }

# run insides logs/
for f in $(ls *-log.txt); do min=$(egrep -A 1 "(Starting|Finished) run_get_files_projs.sh" $f | tr -d '\r' | { readarray a ; echo $(( ( $(date +%s -d "$(echo ${a[4]} | tr -d '\n')") - $(date +%s -d "$(echo ${a[1]} | tr -d '\n')") ) / 60 )); }); hour=$(($min * 50 / 60)); name=$(echo $f | cut -d'=' -f1); echo "$name,$min,$hour"; done | sort -k3 -k2 -k1 -n -t ','



