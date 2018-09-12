package edu.illinois.cs.dt.tools.diagnosis.detection.filters;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.function.Predicate;

public class VerifyFilter implements Predicate<DependentTest> {
    private static final boolean VERIFY_DTS = Configuration.config().getProperty("dt.verify", true);
    private final Runner runner;

    public VerifyFilter(final Runner runner) {
        this.runner = runner;
    }

    @Override
    public boolean test(final DependentTest dependentTest) {
        if (VERIFY_DTS) {
            return dependentTest.verify(runner);
        } else {
            return true;
        }
    }
}
