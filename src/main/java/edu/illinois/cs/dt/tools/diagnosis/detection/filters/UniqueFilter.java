package edu.illinois.cs.dt.tools.diagnosis.detection.filters;

import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

public class UniqueFilter implements Predicate<DependentTest> {
    private final Set<String> prevTests = new HashSet<>();

    @Override
    public boolean test(final DependentTest dependentTest) {
        final boolean found = prevTests.contains(dependentTest.name());
        prevTests.add(dependentTest.name());
        return !found;
    }
}
