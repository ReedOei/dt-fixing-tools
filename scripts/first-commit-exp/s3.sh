#### count (1) number of failures per round and (2) number of rounds with failure per test
# cd ~/complete_output/idflakies/idflakies-vers-comprehensive/tootallnate.java-websocket-fa3909c=_output/Java-WebSocket/detection-results/random/
for i in {0..99}; do python -mjson.tool round$i.json >p$i; done
for i in {0..99}; do awk '/unfiltered/{t=1} {if(t)print}' p$i >u$i; done

for i in {0..99}; do grep "\"name\"" u$i | wc -l; done
for i in {0..99}; do grep "\"name\"" u$i | while read n; do echo $i,$(echo $n | cut -f4 -d\"); done; done | cut -f2 -d, | sort | uniq -c | sort -gr
