package edu.illinois.cs.dt.tools.diagnosis;

import com.google.gson.Gson;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.detection.DetectorPlugin;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPathManager;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPlugin;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerListener;
import edu.illinois.cs.dt.tools.utility.ErrorLogger;
import edu.illinois.cs.dt.tools.utility.PathManager;
import org.apache.maven.project.MavenProject;
import scala.Option;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DiagnoserPlugin extends TestPlugin {
    private MavenProject project;
    private InstrumentingSmartRunner runner;

    public DiagnoserPlugin() {
    }

    @Override
    public void execute(final MavenProject project) {
        this.project = project;

        final Option<Runner> runnerOption = RunnerFactory.from(project);

        final ErrorLogger logger = new ErrorLogger(project);

        System.out.println("DIAGNOSER_MODULE_COORDINATES: " + logger.coordinates());

        logger.runAndLogError(() -> {
            writeSubjectProperties(logger.coordinates());

            if (runnerOption.isDefined()) {
                this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());

                if (!Files.exists(DetectorPathManager.originalOrderPath())) {
                    Files.write(DetectorPathManager.originalOrderPath(), DetectorPlugin.getOriginalOrder(project));
                }

                Configuration.config().properties().setProperty("testrunner.testlistener_class", RunnerListener.class.getCanonicalName());

                diagnose();
            } else {
                final String errorMsg = "Module is not using a supported test framework (probably not JUnit).";
                TestPluginPlugin.info(errorMsg);
                logger.writeError(errorMsg);
            }

            return null;
        });
    }

    private void writeSubjectProperties(final String coordinates) throws IOException {
        final Properties properties = new Properties();
        properties.setProperty("subject.coordinates", coordinates);
        properties.setProperty("subject.name", subjectName(project));

        Files.createDirectories(DiagnoserPathManager.subjectProperties().getParent());
        properties.store(new FileOutputStream(DiagnoserPathManager.subjectProperties().toFile()), "");
    }

    private String subjectName(final MavenProject project) {
        final Path relativePath =
                PathManager.parentPath().getParent().toAbsolutePath()
                        .relativize(project.getBasedir().toPath().toAbsolutePath());

        return relativePath.toString().replace("/", "-");
    }

    private void diagnose() throws Exception {
        results()
                .map(result -> new TestDiagnoser(runner, result).run())
                .flatMap(o -> o.map(Stream::of).orElseGet(Stream::empty))
                .forEach(this::saveResult);
    }

    private void saveResult(final DiagnosisResult diagnosisResult) {
        try {
            final Path path = DiagnoserPathManager.diagnosisResult(diagnosisResult);
            Files.createDirectories(path.getParent());
            Files.write(path, new Gson().toJson(diagnosisResult).getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private Stream<MinimizeTestsResult> results() throws Exception {
        if (Files.exists(MinimizerPathManager.minimized())) {
            return Files.walk(MinimizerPathManager.minimized()).flatMap(p -> {
                try {
                    return Stream.of(MinimizeTestsResult.fromPath(p));
                } catch (IOException ignored) {}

                return Stream.empty();
            });
        } else {
            return detect();
        }
    }

    private Stream<MinimizeTestsResult> detect() throws Exception {
        if (!Files.exists(DetectorPathManager.detectionFile())) {
            if (Configuration.config().getProperty("diagnosis.run_detection", true)) {
                new DetectorPlugin(DetectorPathManager.detectionResults(), runner).execute(project);
            } else {
                throw new NoSuchFileException("File " + DetectorPathManager.detectionFile() + " does not exist and diagnosis.run_detection is set to false");
            }
        }

        return new MinimizerPlugin(runner).runDependentTestFile(DetectorPathManager.detectionFile());
    }
}
