package edu.illinois.cs.dt.tools.detection;

import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.runner.RunnerFactory$;
import com.reedoei.testrunner.testobjects.TestLocator;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class DetectorPlugin extends TestPlugin {
    public static final Path DT_FOLDER = Paths.get("detection-results").toAbsolutePath();

    private final Path outputPath;
    private InstrumentingSmartRunner runner;

    public DetectorPlugin() {
        outputPath = DT_FOLDER.toAbsolutePath();
    }

    public DetectorPlugin(final Path outputPath, final InstrumentingSmartRunner runner) {
        this.outputPath = outputPath;
        this.runner = runner;
    }

    @Override
    public void execute(final MavenProject mavenProject) {
        if (runner == null) {
            this.runner = InstrumentingSmartRunner.fromRunner(RunnerFactory$.MODULE$.from(mavenProject).get());
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
    }
}
