package edu.illinois.cs.dt.tools.minimizer;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class TestMinimizerBuilder {
    private final List<String> testOrder;
    private final String dependentTest;
    private final String classpath;
    private final Path javaAgent;
    private final int verbosity;

    public TestMinimizerBuilder() {
        testOrder = new ArrayList<>();
        dependentTest = "";
        classpath = System.getProperty("java.class.path");
        javaAgent = Paths.get("");
        verbosity = 0;
    }

    public TestMinimizerBuilder(final List<String> testOrder, final String dependentTest,
                                final String classpath, final Path javaAgent,
                                final int verbosity) {
        this.testOrder = testOrder;
        this.dependentTest = dependentTest;
        this.classpath = classpath;
        this.javaAgent = javaAgent;
        this.verbosity = verbosity;
    }

    public TestMinimizerBuilder testOrder(final List<String> testOrder) {
        return new TestMinimizerBuilder(testOrder, dependentTest, classpath, javaAgent, verbosity);
    }

    public TestMinimizerBuilder dependentTest(final String dependentTest) {
        return new TestMinimizerBuilder(testOrder, dependentTest, classpath, javaAgent, verbosity);
    }

    public TestMinimizerBuilder classpath(final String classpath) {
        return new TestMinimizerBuilder(testOrder, dependentTest, classpath, javaAgent, verbosity);
    }

    public TestMinimizerBuilder javaAgent(final Path javaAgent) {
        return new TestMinimizerBuilder(testOrder, dependentTest, classpath, javaAgent, verbosity);
    }

    public TestMinimizerBuilder verbosity(final int verbosity) {
        return new TestMinimizerBuilder(testOrder, dependentTest, classpath, javaAgent, verbosity);
    }

    public TestMinimizer build() throws Exception {
        return new TestMinimizer(testOrder, classpath, dependentTest, javaAgent, verbosity);
    }

    public String classpath() {
        return classpath;
    }
}