package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory$;
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
        final Option<Runner> runnerOption = RunnerFactory$.MODULE$.from(project);

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
        results().forEach(result -> new TestDiagnoser(project, runner, result).run());
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
            new DetectorPlugin(DetectorPathManager.detectionResults(), runner).execute(project);
        }

        return new MinimizerPlugin(runner).runDependentTestFile(DetectorPathManager.detectionFile());
    }
}
