package edu.illinois.cs.dt.tools.runner;

import com.google.common.util.concurrent.UncheckedExecutionException;
import com.reedoei.eunomia.collections.ListUtil;
import org.junit.Test;

import java.util.List;
import java.util.concurrent.TimeoutException;

public class SmartTestRunnerTest {
    private final SmartTestRunner runner = new SmartTestRunner();

    @Test(expected = FlakyTestException.class)
    public void testDetectFlakyTests() throws Throwable {
        final List<String> tests = ListUtil.fromArray("edu.illinois.cs.dt.samples.ExampleHasFlakyTests.testFlakyFileDependent");

        try {
            runner.runOrder(tests);
            runner.runOrder(tests);
        } catch (UncheckedExecutionException e) {
            throw e.getCause();
        }
    }

    @Test(expected = RuntimeException.class)
    public void testErrorsArePrinted() throws Exception {
        final List<String> tests = ListUtil.fromArray("nonexistent.test.name");
        runner.runOrder(tests);
    }

    @Test(expected = TimeoutException.class)
    public void testTimeout() throws Exception {
        final List<String> noTimeoutOrder =
                ListUtil.fromArray("edu.illinois.cs.dt.samples.ExampleTimeoutTests.test1", "edu.illinois.cs.dt.samples.ExampleTimeoutTests.test2");
        final List<String> timeoutOrder =
                ListUtil.fromArray("edu.illinois.cs.dt.samples.ExampleTimeoutTests.test2");

        runner.runOrder(noTimeoutOrder);
        runner.runOrder(timeoutOrder);
    }
}