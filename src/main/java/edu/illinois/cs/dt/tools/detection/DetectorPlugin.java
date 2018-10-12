package edu.illinois.cs.dt.tools.detection;

import com.google.common.util.concurrent.SimpleTimeLimiter;
import com.opencsv.CSVReader;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.utility.GetMavenTestOrder;
import edu.illinois.cs.dt.tools.utility.TestClassData;
import org.apache.maven.project.MavenProject;
import scala.Option;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.nio.file.CopyOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Predicate;

public class DetectorPlugin extends TestPlugin {
    private final Path outputPath;
    private String coordinates;
    private InstrumentingSmartRunner runner;

    // Don't delete this.
    // This is actually used, provided you call this class via Maven (used by the testrunner plugin)
    public DetectorPlugin() {
        outputPath = DetectorPathManager.detectionResults();
    }

    public DetectorPlugin(final Path outputPath, final InstrumentingSmartRunner runner) {
        this.outputPath = outputPath;
        this.runner = runner;
    }

    private void writeError(final Throwable t) {
        try {
            System.out.println("---------------------------------------------------");
            System.out.println("ERROR (WRITE_ERROR_STDOUT_THROWABLE_REEDOEI2): " + coordinates);
            t.printStackTrace();

            t.printStackTrace(new PrintStream(new FileOutputStream(String.valueOf(DetectorPathManager.errorPath()))));
            Files.write(DetectorPathManager.errorPath(), ("\n" + coordinates).getBytes(), StandardOpenOption.APPEND);
        } catch (IOException e) {
            System.out.println("ERROR (FAIL_OUTPUT_ERR_THROWABLE_REEDOEI2): Failed to output error!");
            e.printStackTrace();
            System.out.println("---------------------------------------------------");
            t.printStackTrace();
        }
    }

    private void writeError(final String msg) {
        try {
            System.out.println("---------------------------------------------------");
            System.out.println("ERROR (WRITE_ERROR_STDOUT_STRING_REEDOEI2): " + coordinates);
            System.out.println("Message was:");
            System.out.println(msg);

            Files.write(DetectorPathManager.errorPath(), (msg + "\n").getBytes());
            Files.write(DetectorPathManager.errorPath(), ("\n" + coordinates).getBytes(), StandardOpenOption.APPEND);
        } catch (IOException e) {
            System.out.println("ERROR (FAIL_OUTPUT_ERR_STRING_REEDOEI2): Failed to output error!");
            e.printStackTrace();
            System.out.println("---------------------------------------------------");
            System.out.println("Message was:");
            System.out.println(msg);
        }
    }

    // TODO: copy to eunomia
    private static ListEx<ListEx<String>> csv(final Path path) throws IOException {
        try (final FileInputStream fis = new FileInputStream(path.toAbsolutePath().toString());
             final InputStreamReader isr = new InputStreamReader(fis);
             final CSVReader reader = new CSVReader(isr)) {
            return new ListEx<>(reader.readAll()).map(ListEx::fromArray);
        }
    }

    private static <T> T until(final T t, final Function<T, T> f, final Predicate<T> pred) {
        if (pred.test(t)) {
            return t;
        } else {
            return until(f.apply(t), f, pred);
        }
    }

    private static <T> T untilNull(final T t, final Function<T, T> f) {
        return until(t, f, v -> f.apply(v) == null);
    }

    private MavenProject getMavenProjectParent(MavenProject mavenProject) {
        MavenProject parentProj = mavenProject;
        while (parentProj.getParent() != null && parentProj.getParent().getBasedir() != null) {
            parentProj = parentProj.getParent();
        }
        return parentProj;
    }

    private static ListEx<ListEx<String>> transpose(final ListEx<ListEx<String>> rows) {
        final ListEx<ListEx<String>> result = new ListEx<>();

        final int len = rows.get(0).size();

        for (int i = 0; i < len; i++) {
            final int finalI = i;
            result.add(rows.map(s -> s.get(finalI)));
        }

        return result;
    }

    private long moduleTimeout(final MavenProject mavenProject) throws IOException {
        final MavenProject parent = getMavenProjectParent(mavenProject);

        final Path timeCsv = parent.getBasedir().toPath().resolve("module-test-time.csv");
        Files.copy(timeCsv, DetectorPathManager.detectionResults().resolve("module-test-time.csv"), StandardCopyOption.REPLACE_EXISTING);
        final ListEx<ListEx<String>> csv = csv(timeCsv);

        // Skip the header row, sum the second column to get the total time
        final double totalTime =
                csv.stream()
                        .mapToDouble(row -> Double.valueOf(row.get(1)))
                        .sum();

        // Lookup ourselves in the csv to see how long we took
        final Double moduleTime =
                csv.stream()
                        .filter(row -> row.get(0).equals(coordinates))
                        .findFirst()
                        .map(row -> Double.valueOf(row.get(1)))
                        .orElse(0.0);

        final double timeout =
                Math.max(2.0, moduleTime / totalTime * Configuration.config().getProperty("detector.timeout", 6 * 3600)); // 6 hours

        TestPluginPlugin.mojo().getLog().info("TIMEOUT_CALCULATED: Giving " + coordinates + " " + timeout + " seconds to run for " +
            DetectorFactory.detectorType());

        return (long) timeout; // Allocate time proportionally
    }

    @Override
    public void execute(final MavenProject mavenProject) {
        this.coordinates = mavenProject.getGroupId() + ":" + mavenProject.getArtifactId() + ":" + mavenProject.getVersion();

        try {
            Files.deleteIfExists(DetectorPathManager.errorPath());
            Files.createDirectories(DetectorPathManager.cachePath());
            Files.createDirectories(DetectorPathManager.detectionResults());

            SimpleTimeLimiter.create(Executors.newCachedThreadPool())
                    .callWithTimeout(detectorExecute(mavenProject), moduleTimeout(mavenProject), TimeUnit.SECONDS);
        } catch (Throwable t) {
            writeError(t);
        }
    }

    private <T> Callable<T> detectorExecute(final MavenProject mavenProject) {
        return () -> {
            final Option<Runner> runnerOption = RunnerFactory.from(mavenProject);

            // We need to do two checks to make sure that we can run this project
            // Firstly, we must be able to run it's tests (if we get a runner from the RunnerFactory, we're good)
            // Secondly, there must be some tests (see below)
            if (runnerOption.isDefined()) {
                if (this.runner == null) {
                    this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());
                }

                try {
                    final List<String> tests = getOriginalOrder(mavenProject);

                    // If there are no tests, we can't run a flaky test detector
                    if (!tests.isEmpty()) {
                        Files.createDirectories(outputPath);
                        Files.write(DetectorPathManager.originalOrderPath(), String.join(System.lineSeparator(), tests).getBytes());

                        final Detector detector = DetectorFactory.makeDetector(runner, tests);
                        TestPluginPlugin.mojo().getLog().info("Created dependent test detector (" + detector.getClass() + ").");

                        detector.writeTo(outputPath);
                    } else {
                        final String errorMsg = "Module has no tests, not running detector.";
                        TestPluginPlugin.mojo().getLog().info(errorMsg);
                        writeError(errorMsg);
                    }
                } catch (IOException e) {
                    writeError(e);
                }
            } else {
                final String errorMsg = "Module is not using a supported test framework (probably not JUnit).";
                TestPluginPlugin.mojo().getLog().info(errorMsg);
                writeError(errorMsg);
            }

            return null;
        };
    }

    private List<String> getOriginalOrder(final MavenProject mavenProject) throws IOException {
        if (!Files.exists(DetectorPathManager.originalOrderPath())) {
            final Path surefireReportsPath = Paths.get(mavenProject.getBuild().getDirectory()).resolve("surefire-reports");

            if (!Files.exists(surefireReportsPath)) {
                return new ArrayList<>();
            }

            // TODO: Make this a non-absolute path
            final Path mvnTestLog = Paths.get("/home/awshi2/mvn-test.log");
            final List<TestClassData> testClassData = new GetMavenTestOrder(surefireReportsPath, mvnTestLog).testClassDataList();

            final List<String> tests = new ArrayList<>();

            for (final TestClassData classData : testClassData) {
                for (final String testName : classData.testNames) {
                    tests.add(classData.className + "." + testName);
                }
            }

            return tests;
        } else {
            return Files.readAllLines(DetectorPathManager.originalOrderPath());
        }
    }
}
