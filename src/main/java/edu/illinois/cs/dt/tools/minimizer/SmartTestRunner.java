package edu.illinois.cs.dt.tools.minimizer;

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

// TODO: Make it keep track of much more stuff like timeouts and have it actually do the timeout stuff.
public class SmartTestRunner {
    public SmartTestRunner(final boolean failOnFlakyTests) {

    }

    private static boolean correctTestsRan(final List<String> order, final TestExecResult result) {
        return new HashSet<>(order).equals(result.getNameToResultsMap().keySet());
    }

    public static TestExecResult runOrder(final String classpath, final List<String> order) {
        final CapturedOutput<TestExecResult> capture =
                new CaptureOutStream<>(() -> new FixedOrderRunner(classpath, order).run().getExecutionRecords().get(0))
                        .run();

        final Optional<TestExecResult> result = capture.value();

        if (result.isPresent() && correctTestsRan(order, result.get())) {
            return result.get();
        } else {
            final Path errorPath = Paths.get(order.get(order.size() - 1) + "-error-log.txt");

            System.out.println("[ERROR] An exception occurred while running the order: " + order);
            System.out.println("[ERROR] The full output has been written to: " + errorPath);

            try {
                Files.write(errorPath, capture.stringOutput().getBytes());
            } catch (IOException e) {
                e.printStackTrace();
                System.out.println("[ERROR]: Could not write file. Writing to stout:");

                System.out.println(capture.stringOutput());
            }

            throw new RuntimeException(capture.error());
        }
    }
}
