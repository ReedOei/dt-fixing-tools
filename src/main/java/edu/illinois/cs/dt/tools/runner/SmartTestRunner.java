package edu.illinois.cs.dt.tools.runner;

import com.google.common.util.concurrent.SimpleTimeLimiter;
import com.google.common.util.concurrent.TimeLimiter;
import com.reedoei.eunomia.io.capture.CaptureOutStream;
import com.reedoei.eunomia.io.capture.CapturedOutput;
import com.reedoei.eunomia.util.Util;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.runners.FixedOrderRunner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class SmartTestRunner {
    private static final SmartTestRunner master = new SmartTestRunner();
    private final Path javaAgent;

    public static SmartTestRunner master() {
        return master;
    }

    private final TestInfoStore infoStore = new TestInfoStore();
    private final String classpath;

    public SmartTestRunner() {
        this(System.getProperty("java.class.path"));
    }

    public SmartTestRunner(final String classpath) {
        this(classpath, Paths.get(""));
    }

    public SmartTestRunner(final String classpath, final Path javaAgent) {
        this.classpath = classpath;
        this.javaAgent = javaAgent;
    }

    private boolean correctTestsRan(final List<String> order, final TestExecResult result) {
        return new HashSet<>(order).equals(result.getNameToResultsMap().keySet());
    }

    @SafeVarargs
    public final TestResult runOrder(final List<String>... orders) throws Exception {
        return runOrder(Arrays.stream(orders).reduce(new ArrayList<>(), Util::prependAll));
    }

    public TestResult runOrder(final List<String> order) throws Exception {
        final TimeLimiter limiter = SimpleTimeLimiter.create(Executors.newSingleThreadExecutor());

//        System.out.println();
//        System.out.println("Running: " + order);

        final long timeout = infoStore.getTimeout(order);
        final String endTime = LocalDateTime.now().plusSeconds(timeout).toString();

        System.out.printf(" Running %d tests until %s", order.size(), endTime);

        return limiter.callWithTimeout(runner(classpath, order), infoStore.getTimeout(order), TimeUnit.SECONDS);
    }

    private Callable<TestResult> runner(String classpath, List<String> order) {
        return () -> {
            final CapturedOutput<TestExecResult> capture =
                    new CaptureOutStream<>(() -> new FixedOrderRunner(classpath, order, javaAgent.toString()).run().getExecutionRecords().get(0))
                            .run();

            final Optional<TestExecResult> result = capture.value();

            if (result.isPresent() && correctTestsRan(order, result.get())) {
                return handleResult(order, result.get());
            } else {
                return handleError(order, capture);
            }
        };
    }

    private TestResult handleResult(final List<String> order, final TestExecResult result) {
        infoStore.update(order, result);
        return new TestResult(result);
    }

    private TestResult handleError(final List<String> order, final CapturedOutput<TestExecResult> capture) {
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

        if (capture.error() instanceof RuntimeException) {
            throw (RuntimeException)capture.error();
        } else {
            throw new RuntimeException(capture.error());
        }
    }

    public double averageTestTime() {
        return infoStore.averageTime();
    }
}
