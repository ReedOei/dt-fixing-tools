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
    private Path javaAgent;
    private InstrumentingSmartRunner runner;
    private String coordinates;

    public DiagnoserPlugin() {
    }

    public static String cp() {
        final URLClassLoader contextClassLoader = (URLClassLoader)Thread.currentThread().getContextClassLoader();
        return String.join(File.pathSeparator,
                Arrays.stream(contextClassLoader.getURLs())
                      .map(URL::getPath)
                      .collect(Collectors.toList()));
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

    @Override
    public void execute(final MavenProject project) {
        this.project = project;
        this.coordinates = project.getGroupId() + ":" + project.getArtifactId() + ":" + project.getVersion();

        System.out.println("DIAGNOSER_MODULE_COORDINATES: " + coordinates);

        this.javaAgent = Paths.get(Configuration.config().getProperty("dtfixingtools.javaagent", ""));
        final Option<Runner> runnerOption = RunnerFactory.from(project);

        try {
            writeSubjectProperties();

            if (runnerOption.isDefined()) {
                this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());

                if (!Files.exists(DetectorPathManager.originalOrderPath())) {
                    Files.write(DetectorPathManager.originalOrderPath(), DetectorPlugin.getOriginalOrder(project));
                }

                Configuration.config().properties().setProperty("testrunner.testlistener_class", RunnerListener.class.getCanonicalName());

                try {
                    diagnose();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            } else {
                TestPluginPlugin.mojo().getLog().info("Module is not using a supported test framework (probably not JUnit)");
            }
        } catch (Throwable t) {
            writeError(t);
        } finally {
            final Path failingOutput = project.getBasedir().toPath().resolve("failing-test-output");
            final Path failingOutputDest = DiagnoserPathManager.cachePath().resolve("failing-test-output");

            if (Files.exists(failingOutput)) {
                try {
                    if (Files.exists(failingOutputDest)) {
                        Files.copy(failingOutput, failingOutputDest, StandardCopyOption.REPLACE_EXISTING);
                    } else {
                        Files.copy(failingOutput, failingOutputDest);
                    }
                } catch (IOException ignored) {}
            }
        }
    }

    private void writeSubjectProperties() throws IOException {
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
