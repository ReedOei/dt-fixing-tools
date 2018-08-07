package edu.illinois.cs.dt.tools.diagnosis.detection.filters;

import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.function.Predicate;

public class FlakyFilter implements Predicate<DependentTest> {
    private final SmartTestRunner runner;

    public FlakyFilter(final SmartTestRunner runner) {
        this.runner = runner;
    }

    @Override
    public boolean test(final DependentTest dependentTest) {
        return !runner.isFlaky(dependentTest.name());
    }
}
