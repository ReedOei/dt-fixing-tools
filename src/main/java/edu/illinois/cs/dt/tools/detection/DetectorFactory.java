package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.runner.Runner;

import java.util.List;

public class DetectorFactory {
    public static final int ROUNDS = Configuration.config().getProperty("dt.randomize.rounds", 20);

    public static String detectorType() {
        return Configuration.config().getProperty("detector.detector_type", "random");
    }

    public static Detector makeDetector(final Runner runner, final List<String> tests) {
        if (detectorType().startsWith("random")) {
            return new RandomDetector(detectorType(), runner, ROUNDS, tests);
        } else if (detectorType().startsWith("reverse")) {
            return new ReverseDetector(runner, ROUNDS, detectorType(), tests);
        } else if (detectorType().equals("flaky")) {
            return new FlakyDetector(runner, ROUNDS, tests);
        }

        return new RandomDetector("random", runner, ROUNDS, tests);
    }
}
