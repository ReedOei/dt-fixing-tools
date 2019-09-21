package edu.illinois.cs.dt.tools.utility;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.detection.DetectorPlugin;
import edu.illinois.cs.dt.tools.fixer.JavaMethod;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.project.MavenProject;
import scala.Option;
import sun.security.pkcs11.Secmod;

/**
 * Created by winglam on 9/18/19.
 */

public class GetTestFilePlugin extends TestPlugin {

    private MavenProject project;

    // Don't delete. Need a default constructor for TestPlugin
    public GetTestFilePlugin() {
    }

    private String classpath() throws DependencyResolutionRequiredException {
        final List<String> elements = new ArrayList<>(project.getCompileClasspathElements());
        elements.addAll(project.getRuntimeClasspathElements());
        elements.addAll(project.getTestClasspathElements());

        return String.join(File.pathSeparator,
                           elements);
    }

    @Override
    public void execute(final MavenProject project) {
        this.project = project;

        final Option<Runner> runnerOption = RunnerFactory.from(project);
        final ErrorLogger logger = new ErrorLogger(project);

        System.out.println("GetTestFile_COORDINATES: " + logger.coordinates());

        logger.runAndLogError(() -> {
            logger.writeSubjectProperties();

            if (runnerOption.isDefined()) {

                if (!Files.exists(DetectorPathManager.originalOrderPath())) {
                    Files.write(DetectorPathManager.originalOrderPath(),
                                DetectorPlugin.getOriginalOrder(project));
                }

                final List<String> tests = DetectorPlugin.getOriginalOrder(project);

                List<String> outputStrs = new ArrayList<>();
                for (String test : tests) {
                    JavaMethod javaMethod = JavaMethod.find(test,
                                                            testSources(),
                                                            classpath()).get();
                    final String outputStr = test + "," + javaMethod.javaFile().path().toString();
                    outputStrs.add(outputStr);
                }

                try {
                    final Path outputFile = Paths.get(ModuleTestTimePlugin.getMavenProjectParent(project).getBasedir().getAbsolutePath(),
                                                      "test-to-file.csv");
                    Files.write(outputFile,
                                outputStrs,
                                StandardCharsets.UTF_8,
                                Files.exists(outputFile) ? StandardOpenOption.APPEND : StandardOpenOption.CREATE);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            } else {
                final String errorMsg = "Module is not using a supported test framework (probably not JUnit).";
                TestPluginPlugin.info(errorMsg);
                logger.writeError(errorMsg);
            }

            return null;
        });
    }

    private List<Path> testSources() throws IOException {
        final List<Path> testFiles = new ArrayList<>();
        try (final Stream<Path> paths = Files.walk(Paths.get(project.getBuild().getTestSourceDirectory()))) {
            paths.filter(Files::isRegularFile)
                    .forEach(testFiles::add);
        }
        return testFiles;
    }
}

