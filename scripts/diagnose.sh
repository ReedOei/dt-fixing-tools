#!/usr/bin/env bash

# Usage: bash diagnose.sh path/of/dir/containing/dt/lists
# Should be run in the target folder on the desired subject, after having run:
# $ mvn compile test-compile dependency:copy-dependencies

DT_LOC="$1"

OUTPUT_DIR="minimized/"

CLASSPATH="/home/roei/Java/dtfixingtools/target/dt-fixing-tools-1.0.0-SNAPSHOT.jar:/home/roei/Java/dtfixingtools/target/dependency/*:/home/roei/Java/dtfixingtools/lib/*:"
SUBJ_CLASSPATH="classes/:test-classes/:dependency/*:"
JAVA_AGENT="/home/roei/Java/dtfixingtools/lib/dtdetector-1.2.1.jar"

# NOTE: JAVA_HOME libraries must come first in classpath (not sure why, but Soot fails otherwise).

if [[ ! -z "$DT_LOC" ]]; then
    if [[ "$DT_LOC" =~ .json ]]; then
        java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.Diagnoser -cp "$SUBJ_CLASSPATH" --minimized "$DT_LOC" --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"
    else
        java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.Diagnoser -cp "$SUBJ_CLASSPATH" --dtFolder "$DT_LOC" --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"
    fi
else
    java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.Diagnoser -cp "$SUBJ_CLASSPATH" --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"
fi

