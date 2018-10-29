package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class TestMinimizerBuilder {
    private final List<String> testOrder;
    private final String dependentTest;
    private InstrumentingSmartRunner runner;

    public TestMinimizerBuilder(final InstrumentingSmartRunner runner) {
        this.runner = runner;

        testOrder = new ArrayList<>();
        dependentTest = "";
    }

    public TestMinimizerBuilder(final List<String> testOrder, final String dependentTest,
                                final InstrumentingSmartRunner runner) {
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

    public TestMinimizerBuilder runner(final InstrumentingSmartRunner runner) {
        return new TestMinimizerBuilder(testOrder, dependentTest, runner);
    }

    public TestMinimizer build() throws Exception {
        return new TestMinimizer(testOrder, runner, dependentTest);
    }
}