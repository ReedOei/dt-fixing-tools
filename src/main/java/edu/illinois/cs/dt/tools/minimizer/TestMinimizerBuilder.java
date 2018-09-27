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

    public TestMinimizerBuilder(final Runner runner) {
        this.runner = runner;

        testOrder = new ArrayList<>();
        dependentTest = "";
    }

    public TestMinimizerBuilder(final List<String> testOrder, final String dependentTest,
                                final Runner runner) {
        this.testOrder = testOrder;
        this.dependentTest = dependentTest;
        this.runner = runner;
    }

    public TestMinimizerBuilder testOrder(final List<String> testOrder) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner);
    }

    public TestMinimizerBuilder dependentTest(final String dependentTest) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner);
    }

    public TestMinimizerBuilder runner(final Runner runner) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner);
    }

    public TestMinimizer build() throws Exception {
        return new TestMinimizer(testOrder, runner, dependentTest);
    }
}