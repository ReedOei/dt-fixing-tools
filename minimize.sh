#!/usr/bin/env bash

# Usage: bash minimize.sh path/of/dir/containing/dt/lists
# Will write out the minimized dt-lists to "minimized/"
# Should be in the target folder on the desired subject, after having run:
# $ mvn compile test-compile dependency:copy-dependencies

DT_LOC="$1"

OUTPUT_DIR="minimized/"

CLASSPATH="/home/roei/Java/dtfixingtools/target/dt-fixing-tools-1.0.0-SNAPSHOT.jar:/home/roei/Java/dtfixingtools/target/dependency/*:"
SUBJ_CLASSPATH="classes/:test-classes/:dependency/*:"

mkdir -p "$OUTPUT_DIR"
if [[ -d "$DT_LOC" ]]; then
    java -cp $CLASSPATH edu.illinois.cs.dt.tools.minimizer.MinimizeTestList --classpath $SUBJ_CLASSPATH --dtFolder "$DT_LOC" --outputDir "$OUTPUT_DIR"
else
    java -cp $CLASSPATH edu.illinois.cs.dt.tools.minimizer.MinimizeTestList --classpath $SUBJ_CLASSPATH --dtFile "$DT_LOC" --outputDir "$OUTPUT_DIR"
fi

