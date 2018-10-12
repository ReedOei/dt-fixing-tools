package edu.illinois.cs.dt.tools.utility;

import com.reedoei.testrunner.mavenplugin.TestPlugin;

import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;

import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;

/**
 * Created by winglam on 10/11/18.
 */
public class ModuleTestTimePlugin  extends TestPlugin {
    private final Path outputPath;
    private String coordinates;
    private InstrumentingSmartRunner runner;

    // Don't delete this.
    // This is actually used, provided you call this class via Maven (used by the testrunner plugin)
    public ModuleTestTimePlugin() {
        outputPath = DetectorPathManager.detectionResults();
    }

    public ModuleTestTimePlugin(final Path outputPath, final InstrumentingSmartRunner runner) {
        this.outputPath = outputPath;
        this.runner = runner;
    }

    @Override
    public void execute(final MavenProject mavenProject) {
        this.coordinates = mavenProject.getGroupId() + ":" + mavenProject.getArtifactId() + ":" + mavenProject.getVersion();

        final Path surefireReportsPath = Paths.get(mavenProject.getBuild().getDirectory()).resolve("surefire-reports");
        final Path mvnTestLog = Paths.get("/home/awshi2/mvn-test.log");
        try {
            final List<TestClassData> testClassData = new GetMavenTestOrder(surefireReportsPath, mvnTestLog).testClassDataList();


            final Path outputFile = Paths.get(getMavenProjectParent(mavenProject).getBasedir().getAbsolutePath(),
                                              "mvn_module_test_time.log");
            double totalTime = 0;
            for (TestClassData data : testClassData) {
                totalTime += data.classTime;
            }

            String outputStr = coordinates + "," + totalTime;

            Files.write(outputFile, Arrays.asList(outputStr), StandardCharsets.UTF_8,
                        Files.exists(outputFile) ? StandardOpenOption.APPEND : StandardOpenOption.CREATE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private MavenProject getMavenProjectParent(MavenProject mavenProject) {
        MavenProject parentProj = mavenProject;
        while (parentProj.getParent() != null) {
            parentProj = parentProj.getParent();
        }
        return parentProj;
    }
}
