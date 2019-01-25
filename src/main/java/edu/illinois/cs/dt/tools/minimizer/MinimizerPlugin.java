package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

public class MinimizerPlugin extends TestPlugin {
    private TestMinimizerBuilder builder;
    private InstrumentingSmartRunner runner;

    /**
     * This will clear all the cached test runs!
     * Be careful when calling!
     */
    public MinimizerPlugin() {
    }

    public MinimizerPlugin(final InstrumentingSmartRunner runner) {
        super();
        this.runner = runner;
        this.builder = new TestMinimizerBuilder(runner);
    }

    private Stream<TestMinimizer> fromDtList(final Path path) {
        TestPluginPlugin.info("Creating minimizers for file: " + path);

        try {
            final DependentTestList dependentTestList = DependentTestList.fromFile(path);
            if (dependentTestList == null) {
                throw new IllegalArgumentException("Dependent test list file is empty");
            }

            return dependentTestList.dts().stream()
                    .flatMap(dt -> dt.minimizers(builder, runner));
        } catch (IOException e) {
            return Stream.empty();
        }
    }

    public Stream<MinimizeTestsResult> runDependentTestFolder(final Path dtFolder) throws IOException {
        return Files.walk(dtFolder)
                .filter(p -> Files.isRegularFile(p))
                .flatMap(this::runDependentTestFile);
    }

    public Stream<MinimizeTestsResult> runDependentTestFile(final Path dtFile) {
        return fromDtList(dtFile).flatMap(minimizer -> {
            try {
                final MinimizeTestsResult result = minimizer.get();
                result.save();
                return Stream.of(result);
            } catch (Exception e) {
                e.printStackTrace();
            }

            return Stream.empty();
        });
    }

    @Override
    public void execute(final MavenProject project) {
        this.runner = InstrumentingSmartRunner.fromRunner(RunnerFactory.from(project).get());
        this.builder = new TestMinimizerBuilder(runner);

        StreamUtil.seq(runDependentTestFile(DetectorPathManager.detectionFile()));
    }
}
