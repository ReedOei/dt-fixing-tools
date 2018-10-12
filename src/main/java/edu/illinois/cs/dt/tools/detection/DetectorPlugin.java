package edu.illinois.cs.dt.tools.detection;

import com.google.common.util.concurrent.SimpleTimeLimiter;
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

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

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

    private long moduleTimeout(final MavenProject mavenProject) {
        return Configuration.config().getProperty("detector.timeout", 6 * 3600); // 6 hours
    }

    @Override
    public void execute(final MavenProject mavenProject) {
        this.coordinates = mavenProject.getGroupId() + ":" + mavenProject.getArtifactId() + ":" + mavenProject.getVersion();

        try {
            Files.deleteIfExists(DetectorPathManager.errorPath());
            Files.createDirectories(DetectorPathManager.cachePath());

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
