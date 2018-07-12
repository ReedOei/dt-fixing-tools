#!/usr/bin/env bash

# Usage: bash instrument.sh TARGET_FOLDER

TARGET_FOLDER="$1"

CLASSPATH="/home/roei/Java/dtfixingtools/target/classes/:/home/roei/Java/dtfixingtools/target/test-classes/:/home/roei/Java/dtfixingtools/target/dependency/*:" #:/home/roei/Java/dtfixingtools/lib/*:"
SUBJ_CLASSPATH="classes/:test-classes/:dependency/*:"

rm -rf "output"
mkdir "output"

# Instrument the classes and test classes
java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH:" edu.illinois.cs.dt.tools.diagnosis.instrumentation.Instrumentation --soot-cp "$SUBJ_CLASSPATH:$JAVA_HOME/jre/lib/*" --input-dir "$TARGET_FOLDER/classes" --output-dir "output/"
java -cp "$JAVA_HOME/jre/lib/*:$CLASSPATH" edu.illinois.cs.dt.tools.diagnosis.instrumentation.Instrumentation --soot-cp "$SUBJ_CLASSPATH:$JAVA_HOME/jre/lib/*" --input-dir "$TARGET_FOLDER/test-classes" --output-dir "output/"

# Run the instrumented tests
java -cp "sootOutput/:dependency/*:$CLASSPATH:" edu.illinois.cs.dt.tools.runner.SimpleRunner --tests "$TARGET_FOLDER/test-classes"

