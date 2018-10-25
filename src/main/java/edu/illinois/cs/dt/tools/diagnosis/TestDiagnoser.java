package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.collections.PairStream;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutedField;
import edu.illinois.cs.dt.tools.diagnosis.pollution.Pollution;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.project.MavenProject;
import scala.util.Try;

import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

public class TestDiagnoser {
    private final StaticTracer tracer; // The static fields used by the dependent tryRewrite.
    private final MinimizeTestsResult minimized;

    private final InstrumentingSmartRunner runner;

    public TestDiagnoser(final InstrumentingSmartRunner r, final MinimizeTestsResult minimized) {
        this.runner = r;
        this.minimized = minimized;

        System.out.println();
        System.out.println("-----------------------------------------------------------------");
        System.out.println("Running diagnoser for " + minimized.dependentTest() + " (expected result in this order: " + minimized.expected() + ")");
        System.out.println(minimized.deps().size() + " known dependencies.");

        this.tracer = new StaticFieldInfo(runner, minimized).get();
    }

    public Optional<DiagnosisResult> run() {
        try {
            final Map<String, PollutedField> pollutions =
                    new Pollution(runner, minimized).findPollutions(tracer.staticFields());

            System.out.println("All polluted fields are: " + pollutions.keySet());

            final RewritingResultContainer rewritingResults =
                    new RewritingResultContainer(PairStream.fromMap(pollutions)
                            .mapToStream(this::tryRewrite)
                            .flatMap(Function.identity()));

            System.out.println("The causes are: " + rewritingResults.causes());
            System.out.println("-----------------------------------------------------------------");

            return Optional.of(new DiagnosisResult(minimized, pollutions, rewritingResults));
        } catch (Exception e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    private Stream<RewritingResult> tryRewrite(final String fieldName, final PollutedField field) {
        try {
            return Stream.of(StaticTracer.inMode(TracerMode.REWRITE, () -> {
                Configuration.config().properties().setProperty("statictracer.rewrite.test_name", minimized.dependentTest());
                Configuration.config().properties().setProperty("statictracer.rewrite.field", fieldName);
                Configuration.config().properties().setProperty("statictracer.rewrite.value", field.withoutDepsVal());

                System.out.println("Resetting " + fieldName + " to " +
                        StringUtils.abbreviate(String.valueOf(field.withoutDepsVal()), 50));

                final Try<TestRunResult> testRunResult = runner.runList(minimized.withDeps());
                final TestResult testResult = testRunResult.get().results().get(minimized.dependentTest());

                System.out.println("REWRITE_RUN (" + RunnerPathManager.runResultPath(testRunResult.get(), "output") + "): " + testResult.result());
                System.out.println("NO_REWRITE (" + RunnerPathManager.runResultPath(minimized.expectedRun(), "output") + "): " + minimized.expected());

                return new RewritingResult(fieldName, field, testRunResult.get(), testResult.result(), minimized.expected());
            }));
        } catch (Exception e) {
            e.printStackTrace();
        }

        return Stream.empty();
    }
}
