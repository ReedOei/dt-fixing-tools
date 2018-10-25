package edu.illinois.cs.dt.tools.runner;

import com.reedoei.testrunner.execution.JUnitTestRunner;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import org.junit.runner.Description;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class RunnerListener extends RunListener {
    @Override
    public void testFinished(final Description description) throws Exception {
        final String trackerPath = System.getProperty("statictracer.tracer_path", "");

        if (!"".equals(trackerPath)) {
            final Path path = Paths.get(trackerPath).resolve(JUnitTestRunner.fullName(description));
            Files.createDirectories(path.getParent());
            StaticTracer.output(String.valueOf(path));
        }
    }

    @Override
    public void testFailure(final Failure failure) throws Exception {
//        failure.getException().printStackTrace();
    }

    @Override
    public void testAssumptionFailure(final Failure failure) {
//        failure.getException().printStackTrace();
    }
}
