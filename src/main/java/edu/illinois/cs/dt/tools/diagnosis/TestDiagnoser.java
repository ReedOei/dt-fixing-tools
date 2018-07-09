package edu.illinois.cs.dt.tools.diagnosis;

import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;

import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class TestDiagnoser {
    private final MinimizeTestsResult minimized;

    private final SmartTestRunner runner;
    private Map<String, DiffContainer> pollutions = new HashMap<>();

    public TestDiagnoser(final String classpath, final Path javaAgent, final MinimizeTestsResult minimized) {
        this.minimized = minimized;

        runner = new SmartTestRunner(classpath, javaAgent);
    }

    private void findStatePollutions(final String testName) {
        try {
            System.out.print("[INFO]");
            pollutions.putAll(runner.runOrder(Collections.singletonList(testName)).stateDiffs());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Map<String, DiffContainer> run() {
        minimized.getDeps().forEach(this::findStatePollutions);
        System.out.println();

        for (final String testName : pollutions.keySet()) {
            if (!pollutions.get(testName).getDiffs().isEmpty()) {
                System.out.println("[INFO] --------------------------------------------------");
                System.out.println("[INFO] Found pollutions for test: " + testName);

                pollutions.get(testName).getDiffs().forEach((fieldName, diff) -> {
                    System.out.println();
                    System.out.println(fieldName + ":");
                    System.out.println("    Before: " + diff.getBefore());
                    System.out.println("    After: " + diff.getAfter());
                });
            }
        }

        return pollutions;
    }
}
