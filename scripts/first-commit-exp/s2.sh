cat all-mapping.csv | cut -f9,11 -d, | sort -u | grep -v ",$" | while read l; do sha=$(echo $l | cut -f2 -d, | cut -b-7); name=$(echo $l | cut -f1 -d,); echo === $name $sha >&2; if [[ $(grep "${sha}" /tmp/oo | xargs grep "$name$" | rev | cut -f2 -d/ | rev | sort -u | wc -l) == 2 ]]; then echo =============== $name $sha; grep "${sha}" /tmp/oo | xargs grep "$name$" | rev | cut -f2 -d/ | rev | sort -u; fi; done >fileX

wing@deeds106:~/complete_output/first-sha$ cat all-mapping.csv | cut -f9,11 -d, | sort -u | grep -v ",$" | while read l; do sha=$(echo $l | cut -f2 -d, | cut -b-7); name=$(echo $l | cut -f1 -d,); echo === $name $sha $(grep "${sha}" /tmp/oo | xargs grep "$name$" | rev | cut -f2 -d/ | rev | sort -u | wc -l); done >bar


cat all-mapping.csv | cut -f9,11 -d, | sort -u | grep -v ",$" | while read l; do sha=$(echo $l | cut -f2 -d, | cut -b-7); name=$(echo $l | cut -f1 -d,); echo === $name $sha; grep "${sha}" /tmp/oo | xargs grep "$name$"; done >foo


cat all-mapping.csv | cut -f9,11 -d, | sort -u | grep -v ",$" | while read l; do sha=$(echo $l | cut -f2 -d, | cut -b-7); name=$(echo $l | cut -f1 -d,); echo === $name $sha >&2; if [[ $(grep "${sha}" /tmp/oo | xargs grep "$name$" | rev | cut -f2 -d/ | rev | sort -u | wc -l) == 1 ]]; then mod=$(grep "${sha}" /tmp/oo | xargs grep "$name$" | rev | cut -f2 -d/ | rev | sort -u); echo "$name,$sha,$mod"; fi; done >file1

cat all-mapping.csv | cut -f9,11 -d, | sort -u | grep -v ",$" | while read l; do sha=$(echo $l | cut -f2 -d, | cut -b-7); name=$(echo $l | cut -f1 -d,); echo === $name $sha >&2; if [[ $(grep "${sha}" /tmp/oo | xargs grep "$name$" /dev/null | rev | cut -f2 -d/ | rev | sort -u | wc -l) == 1 ]]; then mod=$(grep "${sha}" /tmp/oo | xargs grep "$name$" /dev/null | rev | cut -f2 -d/ | rev | sort -u); echo "$name,$sha,$mod"; fi; done >file1
