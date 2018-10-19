package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.collections.PairStream;
import com.reedoei.eunomia.subject.classpath.Classpath;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.diagnosis.pollution.Pollution;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.project.MavenProject;
import scala.util.Try;

import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class TestDiagnoser {
    private final StaticTracer tracer; // The static fields used by the dependent test.
    private final MinimizeTestsResult minimized;

    private final MavenProject project;
    private final Runner runner;

    public TestDiagnoser(final MavenProject project, final Runner r, final MinimizeTestsResult minimized) {
        this.project = project;
        this.runner = r;
        this.minimized = minimized;

        this.tracer = new StaticFieldInfo(runner, minimized).get();
    }

    public void run() {
        System.out.println();
        if (minimized.deps().isEmpty()) {
            System.out.println("[INFO] No pollutions: No dependencies for " + minimized.dependentTest() + ".");
        } else {
            System.out.println("[INFO] Polluting dts in dependencies of " + minimized.dependentTest());
        }

        try {
            final Map<String, DiffContainer.Diff> pollutions =
                    new Pollution(runner, minimized).findPollutions(tracer.staticFields());

            System.out.println("-----------------------------------------------------------------");
            System.out.println("All polluted fields are:");
            pollutions.forEach((fieldName, v) -> System.out.println(Pollution.formatDiff(fieldName, v)));

            final List<String> causes = PairStream.fromMap(pollutions)
                    .filter((fieldName, diff) -> {
                        try {
                            return StaticTracer.inMode(TracerMode.REWRITE, () -> {
                                Configuration.config().properties().setProperty("statictracer.rewrite.test", minimized.dependentTest());
                                Configuration.config().properties().setProperty("statictracer.rewrite.field", fieldName);
                                Configuration.config().properties().setProperty("statictracer.rewrite.value", String.valueOf(diff.getAfter()));

                                System.out.println("Trying to reset " + fieldName + " to " +
                                        StringUtils.abbreviate(String.valueOf(diff.getAfter()), 50));

                                final Try<TestRunResult> testRunResult = runner.runList(minimized.withDeps());
                                final TestResult testResult = testRunResult.get().results().get(minimized.dependentTest());

                                System.out.println("After resetting, got: " + testResult.result() + ", without resetting, got: " + minimized.expected());

                                return !minimized.expected().equals(testResult.result());
                            });
                        } catch (Exception e) {
                            e.printStackTrace();
                        }

                        return false;
                    }).peek((causeField, ignored) -> System.out.println("The cause is: " + causeField))
                    .mapToStream((causeField, ignored) -> causeField)
                    .collect(Collectors.toList());

            System.out.println("The causes are: " + causes);
            System.out.println("-----------------------------------------------------------------");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String sootClassPath() {
        return Classpath.build(
                Paths.get("").resolve("sootOutput").toAbsolutePath().toString(),
                project.getBuild().getDirectory() + "/dependency/*") + File.pathSeparator +
                DiagnoserPlugin.cp();
    }
}
