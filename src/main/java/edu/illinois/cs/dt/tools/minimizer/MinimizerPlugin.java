package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

public class MinimizerPlugin extends TestPlugin {
    private TestMinimizerBuilder builder;
    private Runner runner;

    /**
     * This will clear all the cached test runs!
     * Be careful when calling!
     */
    public MinimizerPlugin() {
    }

    public MinimizerPlugin(final Runner runner) {
        super();
        this.runner = runner;
        this.builder = new TestMinimizerBuilder(runner);
    }

    private Stream<TestMinimizer> fromDtList(final Path path) {
        TestPluginPlugin.mojo().getLog().info("Creating minimizers for file: " + path);

        try {
            return DependentTestList.fromFile(path).dts().stream()
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

        final Path order = Paths.get(Configuration.config().getProperty("testminimizer.order", null));
        try {
            final List<String> testOrder = Files.readAllLines(order, Charset.defaultCharset());
            final String dependentTest = Configuration.config().getProperty("testminimizer.dt", testOrder.get(testOrder.size() - 1));

            builder.testOrder(testOrder).dependentTest(dependentTest).build().get().save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
