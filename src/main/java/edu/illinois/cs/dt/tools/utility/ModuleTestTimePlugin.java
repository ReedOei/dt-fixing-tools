package edu.illinois.cs.dt.tools.utility;

import com.reedoei.testrunner.mavenplugin.TestPlugin;
import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Collections;
import java.util.List;

/**
 * Created by winglam on 10/11/18.
 */
public class ModuleTestTimePlugin  extends TestPlugin {
    private String coordinates;

    // Don't delete this.
    // This is actually used, provided you call this class via Maven (used by the testrunner plugin)
    public ModuleTestTimePlugin() {
    }

    @Override
    public void execute(final MavenProject mavenProject) {
        this.coordinates = mavenProject.getGroupId() + ":" + mavenProject.getArtifactId() + ":" + mavenProject.getVersion();

        final Path surefireReportsPath = Paths.get(mavenProject.getBuild().getDirectory()).resolve("surefire-reports");
        final Path mvnTestLog = Paths.get("/home/awshi2/mvn-test.log");
        try {
            final Path outputFile = Paths.get(getMavenProjectParent(mavenProject).getBasedir().getAbsolutePath(),
                    "module-test-time.csv");

            final String outputStr = coordinates + "," + timeFrom(surefireReportsPath, mvnTestLog);

            System.out.println(outputStr);

            Files.write(outputFile, Collections.singletonList(outputStr), StandardCharsets.UTF_8,
                    Files.exists(outputFile) ? StandardOpenOption.APPEND : StandardOpenOption.CREATE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private double timeFrom(final Path path, final Path mvnTestLog) throws IOException {
        if (Files.exists(path)) {
            return new GetMavenTestOrder(path, mvnTestLog).testClassDataList().stream()
                    .mapToDouble(TestClassData::classTime).sum();
        }

        return 0.0;
    }

    private MavenProject getMavenProjectParent(MavenProject mavenProject) {
        MavenProject parentProj = mavenProject;
        while (parentProj.getParent() != null && parentProj.getParent().getBasedir() != null) {
            parentProj = parentProj.getParent();
        }
        return parentProj;
    }
}
