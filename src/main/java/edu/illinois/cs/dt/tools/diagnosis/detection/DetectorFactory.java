package edu.illinois.cs.dt.tools.diagnosis.detection;

import edu.illinois.cs.dt.tools.configuration.Configuration;

import java.util.List;

public class DetectorFactory {
    public static Detector makeDetector(final String classpath, final List<String> tests) throws Exception {
        return new RandomDetector(classpath, Configuration.config().getRounds(), tests);
    }
}
