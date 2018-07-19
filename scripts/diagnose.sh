#!/usr/bin/env bash

# Usage: bash diagnose.sh path/of/dir/containing/dt/lists
# Should be run in the target folder on the desired subject, after having run:
# $ mvn compile test-compile dependency:copy-dependencies

ROOT=$(cd "$(dirname $BASH_SOURCE)"; pwd)

DT_LOC="$1"

OUTPUT_DIR="minimized/"

CLASSPATH="$ROOT/../target/dt-fixing-tools-1.0.0-SNAPSHOT.jar:$ROOT/../target/dependency/*:$ROOT/../lib/*:"
JAVA_AGENT="$ROOT/../target/dependency/dtdetector-1.2.1-SNAPSHOT.jar"

if [[ ! -e "$JAVA_AGENT" ]]; then
    (
        cd "$ROOT/.."
        mvn dependency:copy-dependencies
    )
fi

# NOTE: JAVA_HOME libraries must come first in classpath (not sure why, but Soot fails otherwise).

if [[ ! -z "$DT_LOC" ]]; then
    if [[ "$DT_LOC" =~ .json ]]; then
        java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:$SUBJ_CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.Diagnoser --minimized "$DT_LOC" --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"
    else
        java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:$SUBJ_CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.Diagnoser --dtFolder "$DT_LOC" --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"
    fi
else
    java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:$SUBJ_CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.Diagnoser --outputDir "$OUTPUT_DIR" --javaagent "$JAVA_AGENT"
fi

