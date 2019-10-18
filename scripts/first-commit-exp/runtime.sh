find -name "*_output-log.txt" |
    egrep -A 1 "(Starting|Finished) run_get_files_tests.sh" doanduyhai.achilles-ac85374\=info.archinnov.achilles.it.TestEntityWithSASIIndices.should_search_using_like_non_tokenizer_output-log.txt | tr -d '\r' | { readarray a ; echo $(( ( $(date +%s -d "$(echo ${a[4]} | tr -d '\n')") - $(date +%s -d "$(echo ${a[1]} | tr -d '\n')") ) / 60 )); }
