package edu.illinois.cs.dt.tools.minimizer.cleaner;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.testrunner.configuration.Configuration;
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

    public boolean confirm(final InstrumentingSmartRunner runner) {
        // TODO: Implement
        return true;
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
