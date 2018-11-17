package edu.illinois.cs.dt.tools.minimizer.cleaner;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

import java.util.Objects;

public class CleanerGroup {
    private static final int VERIFY_COUNT = Configuration.config().getProperty("dt.diagnosis.cleaners.verify_count", 1);

    private final String dependentTest;
    private final ListEx<String> cleanerTests;

    public CleanerGroup(final String dependentTest, final ListEx<String> cleanerTests) {
        this.dependentTest = dependentTest;
        this.cleanerTests = cleanerTests;
    }

    public boolean confirm(final InstrumentingSmartRunner runner,
                           final ListEx<String> deps,
                           final Result expected, final Result isolationResult) {
        final ListEx<String> withCleanerOrder = new ListEx<>(deps);
        withCleanerOrder.addAll(cleanerTests);
        withCleanerOrder.add(dependentTest);

        final ListEx<String> withoutCleanerOrder = new ListEx<>(deps);
        withoutCleanerOrder.add(dependentTest);

        for (int i = 0; i < VERIFY_COUNT; i++) {
            System.out.printf("Confirming cleaner group (%d of %d) for %s: %s%n", i, VERIFY_COUNT, dependentTest, cleanerTests);

            if (confirmRun("with", runner, isolationResult, withCleanerOrder)) {
                return false;
            }

            if (confirmRun("without", runner, expected, withoutCleanerOrder)) {
                return false;
            }

            final Result withoutCleaner = runner.runList(withoutCleanerOrder).get().results().get(dependentTest).result();

            if (!withoutCleaner.equals(expected)) {
                return false;
            }
        }

        return true;
    }

    private boolean confirmRun(final String runType,
                               final InstrumentingSmartRunner runner,
                               final Result desiredRes, final ListEx<String> order) {
        System.out.printf("Expected %s cleaner result: %s, got: ", runType, desiredRes);

        final Result res = runner.runList(order).get().results().get(dependentTest).result();

        System.out.println(res);

        return !res.equals(desiredRes);
    }

    public String dependentTest() {
        return dependentTest;
    }

    public ListEx<String> cleanerTests() {
        return cleanerTests;
    }

    @Override
    public int hashCode() {
        return Objects.hash(dependentTest, cleanerTests);
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj instanceof CleanerGroup) {
            return dependentTest().equals(((CleanerGroup) obj).dependentTest()) &&
                   cleanerTests().equals(((CleanerGroup) obj).cleanerTests());
        }

        return false;
    }

    @Override
    public String toString() {
        return cleanerTests().toString();
    }
}