package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory$;
import com.reedoei.testrunner.testobjects.TestLocator;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import org.apache.maven.project.MavenProject;
import scala.Option;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class DetectorPlugin extends TestPlugin {
    private final Path outputPath;
    private InstrumentingSmartRunner runner;

    public DetectorPlugin() {
        outputPath = DetectorPathManager.detectionResults();
    }

    public DetectorPlugin(final Path outputPath, final InstrumentingSmartRunner runner) {
        this.outputPath = outputPath;
        this.runner = runner;
    }

    @Override
    public void execute(final MavenProject mavenProject) {
        final Option<Runner> runnerOption = RunnerFactory$.MODULE$.from(mavenProject);

        if (runnerOption.isDefined()) {
            if (this.runner == null) {
                this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());
            }

            final List<String> tests = scala.collection.JavaConverters.bufferAsJavaList(TestLocator.tests(mavenProject).toList().toBuffer());

            try {
                Files.createDirectories(outputPath);

                final Detector detector = DetectorFactory.makeDetector(runner, tests);
                System.out.println("[INFO] Created dependent test detector (" + detector.getClass() + ").");

                detector.writeTo(outputPath);
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            TestPluginPlugin.mojo().getLog().info("Module is not using a supported test framework (probably not JUnit)");
        }
    }
}
