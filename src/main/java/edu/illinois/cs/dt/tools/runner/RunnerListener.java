package edu.illinois.cs.dt.tools.runner;

import com.reedoei.testrunner.execution.JUnitTestRunner;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import org.junit.runner.Description;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

public class RunnerListener extends RunListener {
    @Override
    public void testFinished(final Description description) throws Exception {
        StaticTracer.output(String.valueOf(StaticFieldInfo.STATIC_FIELD_INFO_PATH.resolve(JUnitTestRunner.fullName(description))));
    }

    @Override
    public void testFailure(final Failure failure) throws Exception {
        failure.getException().printStackTrace();
    }

    @Override
    public void testAssumptionFailure(final Failure failure) {
        failure.getException().printStackTrace();
    }
}
