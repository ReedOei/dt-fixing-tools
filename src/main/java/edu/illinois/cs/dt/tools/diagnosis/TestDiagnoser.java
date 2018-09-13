package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.subject.classpath.Classpath;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.Instrumentation;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.diagnosis.pollution.Pollution;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import org.apache.commons.io.FileUtils;
import org.apache.maven.project.MavenProject;
import scala.Option;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;

public class TestDiagnoser {
    private final StaticTracer tracer; // The static fields used by the dependent test.
    private final MinimizeTestsResult minimized;

//    private final PollutionContainer pollutionContainer;

    private final MavenProject project;
    private final Runner runner;

    public TestDiagnoser(final MavenProject project, final Runner r, final MinimizeTestsResult minimized) {
        this.project = project;
        this.runner = r;
        this.minimized = minimized;

        this.tracer = new StaticFieldInfo(project, runner, minimized).get();

//        this.pollutionContainer = new PollutionContainer(runner, minimized.deps());
    }

    public void run() {
        System.out.println();
        if (minimized.deps().isEmpty()) {
            System.out.println("[INFO] No pollutions: No dependencies for " + minimized.dependentTest() + ".");
        } else {
            System.out.println("[INFO] Polluting dts in dependencies of " + minimized.dependentTest());
        }

//        return pollutionContainer.with(container -> container.print(tracer));


        try {
//            runner.runList(minimized.withDeps());
//            Instrumentation.instrumentProject(project);

            final Optional<Map.Entry<String, DiffContainer.Diff>> rootCauseField =
                    new Pollution(runner, sootClassPath(), minimized).findPollutions(tracer.staticFields())
                    .entrySet().stream()
                    .filter(entry -> {
                        final String fieldName = entry.getKey();
                        final DiffContainer.Diff diff = entry.getValue();

                        try {
                            // This is necessary, otherwise the instrumented code is not executed.
                            // Not sure why
                            FileUtils.deleteDirectory(StaticFieldInfo.STATIC_FIELD_INFO_PATH.toFile());
                            Files.createDirectories(StaticFieldInfo.STATIC_FIELD_INFO_PATH);

                            return StaticTracer.inMode(TracerMode.REWRITE, () -> {
                                Configuration.config().properties().setProperty("statictracer.rewrite.test", minimized.dependentTest());
                                Configuration.config().properties().setProperty("statictracer.rewrite.field", fieldName);
                                Configuration.config().properties().setProperty("statictracer.rewrite.value", String.valueOf(diff.getAfter()));

                                System.out.println("Trying to reset " + fieldName + " to " + diff.getAfter());

                                final Option<TestRunResult> testRunResultOption = runner.runListWithCp(sootClassPath(), minimized.withDeps());

                                if (testRunResultOption.isDefined()) {
                                    final TestResult testResult = testRunResultOption.get().results().get(minimized.dependentTest());

                                    System.out.println("Got " + testResult.result() + ", expected: " + minimized.expected());

                                    return !minimized.expected().equals(testResult.result());
                                }

                                return false;
                            });
                        } catch (Exception e) {
                            e.printStackTrace();
                        }

                        return false;
                    }).findFirst();

            rootCauseField.ifPresent(f -> System.out.println(Pollution.formatDiff(f.getKey(), f.getValue())));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String sootClassPath() {
        return Classpath.build(
                Paths.get("").resolve("sootOutput").toAbsolutePath().toString(),
                project.getBuild().getDirectory() + "/dependency/*") + File.pathSeparator +
                Diagnoser.cp();
    }
}
