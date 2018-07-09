#!/usr/bin/env bash

# Usage: bash diagnose.sh path/of/dir/containing/dt/lists
# Should be run in the target folder on the desired subject, after having run:
# $ mvn compile test-compile dependency:copy-dependencies

DT_LOC="$1"

OUTPUT_DIR="minimized/"

CLASSPATH="/home/roei/Java/dtfixingtools/target/dt-fixing-tools-1.0.0-SNAPSHOT.jar:/home/roei/Java/dtfixingtools/target/dependency/*:/home/roei/Java/dtfixingtools/lib/*:"
SUBJ_CLASSPATH="classes/:test-classes/:dependency/*:"
JAVA_AGENT="/home/roei/Java/dtfixingtools/lib/dtdetector-1.2.1.jar"

java -cp "$CLASSPATH" edu.illinois.cs.dt.tools.diagnosis.Diagnoser -cp "$SUBJ_CLASSPATH" --dtFolder "$DT_LOC" --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"

