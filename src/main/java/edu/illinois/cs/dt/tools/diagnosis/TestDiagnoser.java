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
import edu.illinois.cs.dt.tools.minimizer.PolluterData;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import edu.illinois.cs.dt.tools.utility.PathManager;
import org.apache.commons.lang3.StringUtils;
import scala.util.Try;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
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
        System.out.println("==============================================================");
        System.out.println("Running diagnoser for " + minimized.dependentTest() + " (expected result in this order: " + minimized.expected() + ")");
        if (minimized.polluters().isEmpty()) { // Did it find any polluters? Print for first one
            System.out.println("Found no polluters.");
        }

        this.tracer = new StaticFieldInfo(runner, minimized).get();
    }

    public Optional<DiagnosisResult> run() {
        try {
            final List<PolluterDiagnosis> diagnoses = new ArrayList<>();

            for (final PolluterData polluterData : minimized.polluters()) {
                System.out.println("-------------------------------");
                System.out.println("Diagnosing polluter: " + polluterData.deps());

                final Map<String, PollutedField> pollutions =
                        new Pollution(runner, minimized, polluterData).findPollutions();

                System.out.println("All polluted fields are: " + pollutions.keySet());

                final RewriteTargetContainer rewriteTargetContainer = new RewriteTargetContainer();

                final RewritingResultContainer rewritingResults =
                        new RewritingResultContainer(PairStream.fromMap(pollutions)
                                .mapToStream(this::rewriteTargets)
                                .flatMap(Function.identity())
                                .peek(rewriteTargetContainer::add)
                                .flatMap(target -> tryRewrite(polluterData, target)));

                System.out.println();
                System.out.println("The causes are: " + rewritingResults.causes());

                diagnoses.add(new PolluterDiagnosis(polluterData, pollutions, rewritingResults));
            }

            System.out.println("==============================================================");

            return Optional.of(new DiagnosisResult(minimized, diagnoses));
        } catch (Exception e) {
            e.printStackTrace();
        }

        return Optional.empty();
    }

    private Stream<RewriteTarget> rewriteTargets(final String fieldName, final PollutedField field) {
//        final Diff diff = DiffBuilder.compare(Input.fromString(field.withoutDepsVal()))
//                .withTest(Input.fromString(field.withDepsVal()))
//                .build();

        System.out.println();
        System.out.println("Finding rewrite targets for: " + fieldName);
        final ListEx<RewriteTarget> targets = new ListEx<>();
        targets.add(new RewriteTarget(fieldName, fieldName, field));

        // TODO: Implement this (reset specific parts of static fields). For now it prints far too much useless information
//        for (final Difference difference : diff.getDifferences()) {
//            final String innerFieldName = fieldNameFromXPath(difference.getComparison().getControlDetails().getXPath());
//            final String withoutDepsVal = String.valueOf(difference.getComparison().getControlDetails().getValue());
//            final String withDepsVal = String.valueOf(difference.getComparison().getTestDetails().getValue());
//
////            System.out.println("Found difference. XPath: " + difference.getComparison().getControlDetails().getXPath());
////            System.out.println("    Field name: " + innerFieldName);
////            System.out.println("    withoutDepsValue: " + withoutDepsVal);
////            System.out.println("    withDepsvalue: " + withDepsVal);
//
//            // TODO: Implement this better (specifically rewrite inside StaticTracer)
////            targets.add(new RewriteTarget(fieldName, innerFieldName, field));
//        }

        return targets.stream();
    }

    private Stream<RewritingResult> tryRewrite(final PolluterData polluterData, final RewriteTarget target) {
        try {
            return Stream.of(StaticTracer.inMode(TracerMode.REWRITE, () -> {
                Configuration.config().properties().setProperty("statictracer.rewrite.test_name", minimized.dependentTest());
                Configuration.config().properties().setProperty("statictracer.rewrite.static_field", target.staticFieldName());
                Configuration.config().properties().setProperty("statictracer.rewrite.field", target.fieldName());

                if (target.field().withoutDepsVal() == null) {
                    Configuration.config().properties().setProperty("statictracer.rewrite.value", "<null/>");
                } else {
                    Configuration.config().properties().setProperty("statictracer.rewrite.value", target.field().withoutDepsVal());
                }

                System.out.println("Resetting " + target.fieldName()); // + " to " +
//                        StringUtils.abbreviate(String.valueOf(target.field().withoutDepsVal()), 50));

                final Try<TestRunResult> testRunResult = runner.runList(polluterData.withDeps(minimized.dependentTest()));
                final TestResult testResult = testRunResult.get().results().get(minimized.dependentTest());

                System.out.printf("REWRITE_RUN (%s): %s%n",
                        PathManager.modulePath().relativize(RunnerPathManager.outputPath(testRunResult.get())),
                        testResult.result());
                System.out.printf("NO_REWRITE (%s): %s%n",
                        PathManager.modulePath().relativize(RunnerPathManager.outputPath(minimized.expectedRun())),
                        minimized.expected());

                return new RewritingResult(target, testRunResult.get(), testResult.result(), minimized.expected());
            }, minimized.hash()));
        } catch (Exception e) {
            e.printStackTrace();
        }

        return Stream.empty();
    }

    private String fieldNameFromXPath(final String xPath) {
        if (xPath == null) {
            return "ERROR: xPath was null";
        }

        final String[] components = xPath.split("/");

        final List<String> fqNameComponents = new ArrayList<>();

        for (final String component : components) {
            if (component.indexOf('[') >= 0) {
                final String c = component.substring(0, component.indexOf('['));

                if (!"text()".equals(c)) {
                    fqNameComponents.add(c);
                }
            }
        }

        return String.join(".", fqNameComponents);
    }
}
