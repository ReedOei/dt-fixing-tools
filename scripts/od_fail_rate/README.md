# README on reproducing calculations of OD tests' expected and observed failure rate
A summary of all of the work done by Wing can be found [here](https://docs.google.com/spreadsheets/d/1Vp_IBQ4iS_N09ttJ4L1aPkx5IwdX4-o1g5yEZaHr8Ck).

## Obtaining the number of polluters, cleaners, and expected failure for victim tests
- num_p_c_fail_rate.csv: summary file for each victim the number of polluters and cleaners it has and the victims' expected failure rate based on the class of the victims' polluters and cleaners
```python
python -c'import get_od_fail_info; get_od_fail_info.print_stats("all-polluter-cleaner.csv")' | sort -u > num_p_c_fail_rate.csv ; sed -i '1s/^/test_name,p_count,c_count,expected_fail\n/' num_p_c_fail_rate.csv
```
  - all-polluter-cleaner.csv: summary file of all polluter and cleaner pairs for each victim. Obtained from running
  ```bash
  for f in $(ls all_minimized_results/*.json); do python /home/winglam2/dt-fixing-tools/scripts/python-scripts/find_all_cleaners.py $f ; done  > all-polluter-cleaner.csv
  ```
    - all_minimized_results.zip: contains just the Minimizer's json file output for each OD test. Run the following inside `get_all_polluters_complete_11m_19d_11m_21d_11m_24d` when it is unzipped. Obtained from running
    ```bash
    rm -rf all_minimized_results/ ; mkdir -p all_minimized_results/ ; for f in $(find -name "*.json" | grep "minimized"); do cp $f all_minimized_results/ ; done
    ```
      - get_all_polluters_complete_11m_19d_11m_21d_11m_24d.zip: contains the raw output of our framework when getting all polluters for all victims. Omitted from repo due to its size. Obtained from running
      ```bash
      nohup bash run-pool.sh data/fse_truly_od_test_split/ 2 10000000 "run_minimizer_tool.sh" 8 "" 300 &
      ```
## Obtaining the number of runs that pass/fail when we expect an OD test to based on the known polluters/cleaners for the OD test
- check_od_fails_summary.csv: summary file for each victim containing the number of runs whose results matches what we expect it to be. Obtained from running
```python
python -c'import get_od_fail_info; get_od_fail_info.get_check_od_fails_summary("test-runs-results.csv")' | sort -u > check_od_fails_summary.csv ; sed -i '1s/^/test_name,missing_dep_info,num_runs_with_v,num_runs_total,num_match,num_not_match\n/' check_od_fails_summary.csv
```
  - test-runs-results.csv: summary file for whether a victim's result in its observed runs are what we expect it to be based on the known polluters and cleaners of the victim. Obtained from running
  ```python
  python -c'import get_od_fail_info; get_od_fail_info.check_od_fails("/home/wing/all-polluter-cleaner.csv", "/home/wing/complete_output/idflakies/all_modules_runs", "/home/wing/dt-fixing-tools/scripts/victim-module.csv")' | sort -u > test-runs-results.csv ; sed -i '1s/^/test_name,status,observed_result,expected_result,file_name\n/' test-runs-results.csv
  ```
    - all-polluter-cleaner.csv: see above for what all-polluter-cleaner.csv is and how it is obtained
    - victim-module.csv: generated from `nov-24th-no_gap_icst_2.db` database. This file contains only the victims (brittles are omitted) and tests that we have polluter/cleaner information for. Obtained by running
    ```sql
    select * from (select distinct ftf.test_name,ftf.module from fs_subj_test_raw ftf where test_name in (select test_name from tests_with_polluter UNION ALL select * from tests_with_setter));
    ```
    - all_modules_runs.zip: contains the output of the runs for each module obtained during the ICST 2020 submission. These runs are copied from `/home/wing/complete-output/idflakies` but due to the size of `idflakies`, I only provide this zip file. Obtained from running
    ```bash
    rm -rf all_modules_runs; mkdir -p all_modules_runs; for module in $(cat /home/wing/dt-fixing-tools/scripts/victim-module.csv | cut -d, -f2 | uniq); do location=$(find -type d -name $module); echo -e "===$module\n  $location"; cp -r $location all_modules_runs/; done
    ```
