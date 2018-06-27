package edu.illinois.cs.dt.tools.runner;

import com.google.common.util.concurrent.SimpleTimeLimiter;
import com.google.common.util.concurrent.TimeLimiter;
import com.reedoei.eunomia.io.CaptureOutStream;
import com.reedoei.eunomia.io.CapturedOutput;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.runners.FixedOrderRunner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class SmartTestRunner {
    private final TestInfoStore infoStore = new TestInfoStore();
    private final String classpath;

    public SmartTestRunner() {
        this(System.getProperty("java.class.path"));
    }

    public SmartTestRunner(final String classpath) {
        this.classpath = classpath;
    }

    private boolean correctTestsRan(final List<String> order, final TestExecResult result) {
        return new HashSet<>(order).equals(result.getNameToResultsMap().keySet());
    }

    public TestExecResult runOrder(final List<String> order)
            throws InterruptedException, ExecutionException, TimeoutException {
        final TimeLimiter limiter = SimpleTimeLimiter.create(Executors.newSingleThreadExecutor());
        return limiter.callWithTimeout(runner(classpath, order), infoStore.getTimeout(order), TimeUnit.SECONDS);
    }

    private Callable<TestExecResult> runner(String classpath, List<String> order) {
        return () -> {
            final CapturedOutput<TestExecResult> capture =
                    new CaptureOutStream<>(() -> new FixedOrderRunner(classpath, order).run().getExecutionRecords().get(0))
                            .run();

            final Optional<TestExecResult> result = capture.value();

            if (result.isPresent() && correctTestsRan(order, result.get())) {
                return handleResult(order, result.get());
            } else {
                return handleError(order, capture);
            }
        };
    }

    private TestExecResult handleResult(final List<String> order, final TestExecResult result) {
        infoStore.update(order, result);
        return result;
    }

    private TestExecResult handleError(final List<String> order, final CapturedOutput<TestExecResult> capture) {
        final Path errorPath = Paths.get(order.get(order.size() - 1) + "-error-log.txt");

        System.out.println("[ERROR] An exception occurred while running the order: " + order);
        System.out.println("[ERROR] The full output has been written to: " + errorPath);

        try {
            Files.write(errorPath, capture.stringOutput().getBytes());
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("[ERROR]: Could not write file. Writing to stdout:");

            System.out.println(capture.stringOutput());
        }

        throw new RuntimeException(capture.error());
    }
}
