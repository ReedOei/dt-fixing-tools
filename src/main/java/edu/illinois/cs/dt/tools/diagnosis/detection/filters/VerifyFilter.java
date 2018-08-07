package edu.illinois.cs.dt.tools.diagnosis.detection.filters;

import edu.illinois.cs.dt.tools.configuration.Configuration;
import edu.illinois.cs.dt.tools.runner.data.DependentTest;

import java.util.function.Predicate;

public class VerifyFilter implements Predicate<DependentTest> {
    private static final boolean VERIFY_DTS = Configuration.config().getProperty("dt.verify", true);

    private final String classpath;

    public VerifyFilter(final String classpath) {
        this.classpath = classpath;
    }

    @Override
    public boolean test(final DependentTest dependentTest) {
        if (VERIFY_DTS) {
            return dependentTest.verify(classpath);
        } else {
            return true;
        }
    }
}
