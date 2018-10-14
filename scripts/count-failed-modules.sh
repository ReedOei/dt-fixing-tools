#!/usr/bin/env bash

# Usage: bash count-failed-modules.sh RESULTS_PATH

results_path="$1"

no_poms=$(find "$results_path" -name "*.out" | xargs grep -il "there is no POM in this directory" | wc -l)
echo "\\newcommand{\\numModulesNoPoms}{$no_poms\\xspace}"

no_junit_num=$(find "$results_path" -name "error" | xargs grep -Eil "Probably not JUnit|Module has no tests, not running detector." | sort -u | wc -l)
echo "\\newcommand{\\numModulesNoJUnit}{$no_junit_num\\xspace}"

no_passing_order_num=$(find "$results_path" -name "error" | xargs grep -il "NoPassingOrderException" | sort -u | wc -l)
echo "\\newcommand{\\numModulesNoPassingOrder}{$no_passing_order_num\\xspace}"

incompatible_num=$(find ../../azure-dataset-10-13/ -name "error" | xargs grep -EiL "has no tests|No passing|probably not JUnit" | wc -l)
echo "\\newcommand{\\numModulesIncompatible}{$incompatible_num\\xspace}"

no_compilation_num=$(find "$results_path" -name "original.log" | xargs grep -Eil "org.apache.maven.lifecycle.LifecycleExecutionException: Failed to execute goal|Compilation failure" | sort -u | wc -l)
echo "\\newcommand{\\numModulesNoCompilation}{$no_compilation_num\\xspace}"

no_module_num=$(find "$results_path" -name "error" | wc -l)
echo "\\newcommand{\\numModulesNoTotal}{$no_module_num\\xspace}"

no_proj_num=$(find "$results_path" -name "error" | xargs dirname | xargs dirname | sort -u | wc -l)
echo "\\newcommand{\\numProjsNoTotal}{$no_proj_num\\xspace}"

