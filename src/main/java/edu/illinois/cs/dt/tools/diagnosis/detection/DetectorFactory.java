package edu.illinois.cs.dt.tools.diagnosis.detection;

import edu.illinois.cs.dt.tools.configuration.Configuration;

import java.util.List;

public class DetectorFactory {

    public static final int ROUNDS = Configuration.config().getProperty("dt.randomize.rounds", 20);

    public static Detector makeDetector(final String classpath, final List<String> tests) throws Exception {
        return new RandomDetector(classpath, ROUNDS, tests);
    }
}
