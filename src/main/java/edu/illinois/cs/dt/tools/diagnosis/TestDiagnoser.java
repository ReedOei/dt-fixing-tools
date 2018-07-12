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
        minimized.getDeps().forEach(this::findStatePollutions);

        System.out.println(staticFieldsDt);

        if (pollutions.keySet().stream().anyMatch(testName -> !pollutions.get(testName).getDiffs().isEmpty())) {
            System.out.println();
            System.out.println("[INFO] Polluting tests in dependencies of " + minimized.getDependentTest());
            System.out.println();

            for (final String testName : pollutions.keySet()) {
                if (!pollutions.get(testName).getDiffs().isEmpty()) {
                    System.out.println("[INFO] Found pollutions for test: " + testName);

                    pollutions.get(testName).getDiffs().forEach((fieldName, diff) -> {
                        if (staticFieldsDt.contains(fieldName)) {
                            System.out.println("****************************************");
                            System.out.println("This field is accessed by the test code!");
                            System.out.println("****************************************");
                        }

                        System.out.println(fieldName + ":");
                        System.out.println("    Before: " + diff.getBefore());
                        System.out.println("    After: " + diff.getAfter());
                    });

                    System.out.println();
                }
            }
        } else {
            if (minimized.getDeps().isEmpty()) {
                System.out.println("[INFO] No dependencies for " + minimized.getDependentTest() + ", so no polluting tests found.");
            } else {
                System.out.println("[INFO] No polluting tests in dependencies of: " + minimized.getDependentTest());
            }
        }

        return pollutions;
    }
}
