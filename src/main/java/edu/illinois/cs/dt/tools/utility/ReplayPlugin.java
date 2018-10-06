package edu.illinois.cs.dt.tools.utility;

import com.google.gson.Gson;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import org.apache.maven.project.MavenProject;
import scala.Option;
import scala.util.Try;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ReplayPlugin extends TestPlugin {
    @Override
    public void execute(final MavenProject mavenProject) {
        final Option<Runner> runnerOption = RunnerFactory.from(mavenProject);

        if (runnerOption.isDefined()) {
            final Path replayPath = Paths.get(Configuration.config().getProperty("replay.path"));
            final Path outputPath = Paths.get(Configuration.config().properties().getProperty("replay.output_path"));

            try {
                final Runner runner = runnerOption.get(); // safe because we checked above
                final TestRunResult result = new Gson().fromJson(FileUtil.readFile(replayPath), TestRunResult.class);

                final Try<TestRunResult> testRunResultTry = runner.runList(result.testOrder());

                if (testRunResultTry.isSuccess()) {
                    Files.write(outputPath, new Gson().toJson(testRunResultTry.get()).getBytes());
                } else {
                    TestPluginPlugin.mojo().getLog().error(testRunResultTry.failed().get());
                }
            } catch (IOException e) {
                TestPluginPlugin.mojo().getLog().error(e);
            }
        } else {
            TestPluginPlugin.mojo().getLog().info("Module is not using a supported test framework (probably not JUnit).");
        }
    }
}
