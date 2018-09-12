package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.testrunner.runner.Runner;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class TestMinimizerBuilder {
    private final List<String> testOrder;
    private final String dependentTest;
    private Runner runner;
    private final Path javaAgent;
    private final int verbosity;

    public TestMinimizerBuilder(final Runner runner) {
        this.runner = runner;

        testOrder = new ArrayList<>();
        dependentTest = "";
        javaAgent = Paths.get("");
        verbosity = 0;
    }

    public TestMinimizerBuilder(final List<String> testOrder, final String dependentTest,
                                final Runner runner, final Path javaAgent,
                                final int verbosity) {
        this.testOrder = testOrder;
        this.dependentTest = dependentTest;
        this.runner = runner;
        this.javaAgent = javaAgent;
        this.verbosity = verbosity;
    }

    public TestMinimizerBuilder testOrder(final List<String> testOrder) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner, javaAgent, verbosity);
    }

    public TestMinimizerBuilder dependentTest(final String dependentTest) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner, javaAgent, verbosity);
    }

    public TestMinimizerBuilder runner(final Runner runner) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner, javaAgent, verbosity);
    }

    public TestMinimizerBuilder javaAgent(final Path javaAgent) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner, javaAgent, verbosity);
    }

    public TestMinimizerBuilder verbosity(final int verbosity) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner, javaAgent, verbosity);
    }

    public TestMinimizer build() throws Exception {
        return new TestMinimizer(testOrder, runner, dependentTest, javaAgent, verbosity);
    }
}