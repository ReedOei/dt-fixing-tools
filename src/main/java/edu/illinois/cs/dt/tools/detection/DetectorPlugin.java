package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import com.reedoei.testrunner.testobjects.TestLocator;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.utility.GetMavenTestOrder;
import edu.illinois.cs.dt.tools.utility.TestClassData;
import org.apache.maven.project.MavenProject;
import scala.Option;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class DetectorPlugin extends TestPlugin {
    private final Path outputPath;
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

    @Override
    public void execute(final MavenProject mavenProject) {
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
                    TestPluginPlugin.mojo().getLog().info("Module has no tests, not running detector.");
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            TestPluginPlugin.mojo().getLog().info("Module is not using a supported test framework (probably not JUnit).");
        }
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
