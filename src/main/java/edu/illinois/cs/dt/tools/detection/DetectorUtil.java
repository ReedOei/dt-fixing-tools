package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.stream.Stream;

public class DetectorUtil {
    public static TestRunResult originalResults(final List<String> originalOrder, final Runner runner) {
        final int originalOrderTries = Configuration.config().getProperty("dt.detector.original_order.retry_count", 3);
        final boolean allMustPass = Configuration.config().getProperty("dt.detector.original_order.all_must_pass", true);

        System.out.println("[INFO] Getting original results (" + originalOrder.size() + " tests).");

        TestRunResult origResult = null;

        boolean allPassing = false;
        // Try to run it three times, to see if we can get everything to pass (except for ignored tests)
        for (int i = 0; i < originalOrderTries; i++) {
            origResult = runner.runList(originalOrder).get();
            final Stream<TestResult> values = origResult.results().values().stream();

            try {
                Files.write(DetectorPathManager.originalResultsLog(), (origResult.id() + "\n").getBytes(),
                        Files.exists(DetectorPathManager.originalOrderPath()) ? StandardOpenOption.APPEND : StandardOpenOption.CREATE);
            } catch (IOException ignored) {}

            // Ignored tests will show up as SKIPPED, but that's fine because surefire would've skipped them too
            if (values.allMatch(tr -> tr.result().equals(Result.PASS) || tr.result().equals(Result.SKIPPED))) {
                allPassing = true;
                break;
            }
        }

        if (!allPassing) {
            if (allMustPass) {
                throw new NoPassingOrderException("No passing order for tests (" + originalOrderTries + " runs)");
            } else {
                TestPluginPlugin.info("No passing order for tests (" + originalOrderTries + " runs). Continuing anyway with last run.");
            }
        }

        return origResult;
    }
}
