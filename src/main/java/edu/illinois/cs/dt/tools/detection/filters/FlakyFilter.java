package edu.illinois.cs.dt.tools.detection.filters;

import com.reedoei.testrunner.runner.SmartRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.function.Predicate;

public class FlakyFilter implements Predicate<DependentTest> {
    private final SmartRunner runner;

    public FlakyFilter(final SmartRunner runner) {
        this.runner = runner;
    }

    @Override
    public boolean test(final DependentTest dependentTest) {
        return !runner.info().isFlaky(dependentTest.name());
    }
}
