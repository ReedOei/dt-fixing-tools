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
import org.apache.maven.project.MavenProject;
import scala.Option;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DiagnoserPlugin extends TestPlugin {
    private MavenProject project;
    private Path javaAgent;
    private InstrumentingSmartRunner runner;

    public DiagnoserPlugin() {
    }

    public static String cp() {
        final URLClassLoader contextClassLoader = (URLClassLoader)Thread.currentThread().getContextClassLoader();
        return String.join(File.pathSeparator,
                Arrays.stream(contextClassLoader.getURLs())
                      .map(URL::getPath)
                      .collect(Collectors.toList()));
    }

    @Override
    public void execute(final MavenProject project) {
        this.project = project;

        this.javaAgent = Paths.get(Configuration.config().getProperty("dtfixingtools.javaagent", ""));
        final Option<Runner> runnerOption = RunnerFactory.from(project);

        if (runnerOption.isDefined()) {
            this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());

            Configuration.config().properties().setProperty("testrunner.testlistener_class", RunnerListener.class.getCanonicalName());

            try {
                diagnose();
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            TestPluginPlugin.mojo().getLog().info("Module is not using a supported test framework (probably not JUnit)");
        }
    }

    private void diagnose() throws Exception {
        results()
                .map(result -> new TestDiagnoser(runner, result).run())
                .flatMap(o -> o.isPresent() ? Stream.of(o.get()) : Stream.empty())
                .forEach(this::saveResult);
    }

    private void saveResult(final DiagnosisResult diagnosisResult) {
        try {
            final Path path = DiagnoserPathManager.diagnosisResult(diagnosisResult);
            Files.createDirectories(path.getParent());
            System.out.println(diagnosisResult);
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
