package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;

import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class TestDiagnoser {
    private final Map<String, Set<String>> staticFieldInfo;
    private final Set<String> staticFieldsDt; // The static fields used by the dependent test.
    private final MinimizeTestsResult minimized;

    private final SmartTestRunner runner;
    private Map<String, DiffContainer> pollutions = new HashMap<>();

    public TestDiagnoser(final String classpath, final Path javaAgent,
                         final Map<String, Set<String>> staticFieldInfo, final MinimizeTestsResult minimized) {
        this.staticFieldInfo = staticFieldInfo;
        this.minimized = minimized;

        this.staticFieldsDt = staticFieldInfo.getOrDefault(minimized.getDependentTest(), new HashSet<>());

        runner = new SmartTestRunner(classpath, javaAgent);
    }

    private void findStatePollutions(final String testName) {
        try {
            System.out.print("[INFO] Finding pollutions for: " + testName);
            pollutions.putAll(runner.runOrder(Collections.singletonList(testName)).stateDiffs());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Map<String, DiffContainer> run() {
        if (minimized.getDeps().isEmpty()) {
            System.out.println("[INFO] No dependencies for " + minimized.getDependentTest() + ", so no polluting dts found.");
            return pollutions;
        }

        minimized.getDeps().forEach(this::findStatePollutions);

        if (pollutions.keySet().stream().anyMatch(testName -> !pollutions.get(testName).getDiffs().isEmpty())) {
            System.out.println();
            System.out.println("[INFO] Polluting dts in dependencies of " + minimized.getDependentTest());

            for (final String testName : pollutions.keySet()) {
                if (!pollutions.get(testName).getDiffs().isEmpty()) {
                    System.out.println("[INFO] Found pollution from: " + testName);

                    pollutions.get(testName).getDiffs().forEach((fieldName, diff) -> {
                        if (staticFieldsDt.contains(fieldName)) {
                            System.out.println("**Accessed by test** " + formatDiff(fieldName, diff));
                        } else {
                            System.out.println(formatDiff(fieldName, diff));
                        }
                    });

                    System.out.println();
                }
            }
        } else {
            System.out.println("[INFO] No polluting dts in dependencies of: " + minimized.getDependentTest());
        }

        return pollutions;
    }

    private String formatDiff(final String fieldName, final DiffContainer.Diff diff) {
        return String.format("%s: (%s, %s)", fieldName, diff.getBefore(), diff.getAfter());
    }
}
