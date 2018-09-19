package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory$;
import com.reedoei.testrunner.testobjects.TestLocator;
import edu.illinois.cs.dt.tools.diagnosis.detection.Detector;
import edu.illinois.cs.dt.tools.diagnosis.detection.DetectorFactory;
import edu.illinois.cs.dt.tools.diagnosis.detection.ExecutingDetector;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestList;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerListener;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// TODO: Make all files cache inside of a dir like .dtfixingtools
public class Diagnoser extends TestPlugin {
    private MavenProject project;
    private Path javaAgent;
    private Runner runner;

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
        this.runner = InstrumentingSmartRunner.fromRunner(RunnerFactory$.MODULE$.from(project).get());

        Configuration.config().properties().setProperty("testrunner.testlistener_class", RunnerListener.class.getCanonicalName());

        try {
            diagnose();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void diagnose() throws Exception {
        results().forEach(result -> new TestDiagnoser(project, runner, result).run());
    }

    private Stream<MinimizeTestsResult> results() throws Exception {
        if (Files.exists(Paths.get("minimized"))) {
            return Files.walk(Paths.get("minimized")).flatMap(p -> {
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
        final Path dtFolder = Files.createDirectories(Paths.get("detection-results"));
        final Path dtFile = dtFolder.resolve(ExecutingDetector.DT_LISTS_PATH);

        if (!Files.exists(dtFile)) {
            final List<String> tests = scala.collection.JavaConverters.bufferAsJavaList(TestLocator.tests(project).toList().toBuffer());

            final Detector detector = DetectorFactory.makeDetector(runner, tests);
            System.out.println("[INFO] Created dependent test detector (" + detector.getClass() + ").");
            detector.writeTo(dtFolder);
        }

        return new MinimizeTestList(runner).runDependentTestFile(dtFile);
    }
}
