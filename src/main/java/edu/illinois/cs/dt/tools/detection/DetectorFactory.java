package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

import java.util.List;

public class DetectorFactory {
    public static final int ROUNDS = Configuration.config().getProperty("dt.randomize.rounds", 20);

    public static String detectorType() {
        return Configuration.config().getProperty("detector.detector_type", "random");
    }

    public static Detector makeDetector(final Runner runner, final List<String> tests) {
        return makeDetector(runner, tests, ROUNDS);
    }

    public static Detector makeDetector(final Runner runner, final List<String> tests, final int rounds) {
        if (detectorType().startsWith("random")) {
            return new RandomDetector(detectorType(), runner, Math.min(ROUNDS, rounds), tests);
        } else if (detectorType().startsWith("reverse")) {
            return new ReverseDetector(runner, Math.min(ROUNDS, rounds), detectorType(), tests);
        } else if (detectorType().equals("flaky")) {
            return new FlakyDetector(runner, Math.min(ROUNDS, rounds), tests);
        }

        return new RandomDetector("random", runner, Math.min(ROUNDS, rounds), tests);
    }
}
