#### count the number of rounds per module
function inti() {
(
cd /home/wing/complete_output/first-sha
ls -d */*output | while read d ; do (echo == $d; cd $d; find -type d -maxdepth 1 -mindepth 1 | while read s; do echo $d/$s,$(cd $s; find -name 'round*json' | egrep 'random/|reverse/' | wc -l ); done ); done | grep -v ,0 >../first-sha.csv
)
(
cd /home/wing/complete_output/idflakies
ls -d */*output | while read d ; do (echo == $d; cd $d; find -type d -maxdepth 1 -mindepth 1 | while read s; do echo $d/$s,$(cd $s; find -name 'round*json' | egrep 'random/|reverse/' | wc -l ); done ); done | grep -v ,0 >../idflakies.csv
)
}

# init()
for l in $(tail -n +2 idf_test_to_first_test_w_module.csv); do
  # echo $(echo $l | cut -f3 -d, | cut -b-7).*$(echo $l | cut -f2 -d,)
  echo $(echo $l | cut -f4 -d,),$(grep "$(echo $l | cut -f3 -d, | cut -b-7).*$(echo $l | cut -f2 -d,)" idflakies.csv | cut -f2 -d,)
done
