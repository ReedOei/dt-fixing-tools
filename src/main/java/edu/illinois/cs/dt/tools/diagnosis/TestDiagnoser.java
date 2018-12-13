package edu.illinois.cs.dt.tools.diagnosis;

import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListEx;
import com.reedoei.eunomia.collections.PairStream;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.TestResult;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutedField;
import edu.illinois.cs.dt.tools.diagnosis.pollution.Pollution;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.FieldDiff;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.RewriteTarget;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.RewriteTargetContainer;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.RewritingResult;
import edu.illinois.cs.dt.tools.diagnosis.rewrite.RewritingResultContainer;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.PolluterData;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;
import edu.illinois.cs.dt.tools.utility.MD5;
import edu.illinois.cs.dt.tools.utility.PathManager;
import org.xmlunit.builder.DiffBuilder;
import org.xmlunit.builder.Input;
import org.xmlunit.diff.Diff;
import org.xmlunit.diff.Difference;
import scala.util.Try;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TestDiagnoser {
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

        new StaticFieldInfo(runner, minimized).get();
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

                final RewritingResultContainer rewritingResults =
                        new RewritingResultContainer(
                                makeTargets(PairStream.fromMap(pollutions)
                                .mapToStream(rewriteTargets(polluterData))
                                .flatMap(Function.identity()))
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

    private Stream<RewriteTargetContainer> makeTargets(final Stream<RewriteTarget> targetStream) {
        final List<RewriteTarget> allTargets = targetStream.collect(Collectors.toList());

        return Stream.concat(
                allTargets.stream().map(RewriteTargetContainer::new),
                Stream.of(new RewriteTargetContainer(allTargets)));
    }

    private BiFunction<String, PollutedField, Stream<RewriteTarget>> rewriteTargets(final PolluterData polluterData) {
        return (fieldName, field) -> {
            System.out.println("Finding rewrite targets for: " + fieldName);

            final ListEx<RewriteTarget> targets = new ListEx<>();
            targets.add(new RewriteTarget(fieldName, fieldName, field));

            final ListEx<FieldDiff> diffs = new ListEx<>();

            if (field.withDepsVal() != null && field.withoutDepsVal() != null) {
                final Diff diff = DiffBuilder.compare(Input.fromString(field.withoutDepsVal()))
                        .withTest(Input.fromString(field.withDepsVal()))
                        .build();

                // TODO: Implement this (reset specific parts of static fields).
                for (final Difference difference : diff.getDifferences()) {
                    final String xpath = difference.getComparison().getControlDetails().getXPath();
                    final String innerFieldName = fieldNameFromXPath(difference.getComparison().getControlDetails().getXPath());
                    final String withoutDepsVal = String.valueOf(difference.getComparison().getControlDetails().getValue());
                    final String withDepsVal = String.valueOf(difference.getComparison().getTestDetails().getValue());

                    diffs.add(new FieldDiff(fieldName, xpath, innerFieldName, withDepsVal, withoutDepsVal));

                    // TODO: Implement this better (specifically rewrite inside StaticTracer)
                    //            targets.add(new RewriteTarget(fieldName, innerFieldName, field));
                }
            }

            try {
                saveDiffs(polluterData, fieldName, diffs);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            return targets.stream();
        };
    }

    private void saveDiffs(final PolluterData polluterData, final String fieldName, final ListEx<FieldDiff> diffs)
            throws IOException {
        final String hash = MD5.hashOrder(polluterData.deps());


        final Path path = DiagnoserPathManager.diffsPath(hash, fieldName).toAbsolutePath();
        Files.createDirectories(path.getParent());
        Files.write(path, new Gson().toJson(diffs).getBytes());
    }

    private Stream<RewritingResult> tryRewrite(final PolluterData polluterData, final RewriteTargetContainer targets) {
        try {
            return Stream.of(StaticTracer.inMode(TracerMode.REWRITE, () -> {
                final Path tempFile = Files.createTempFile("rewrite", "container");
                try {
                    System.out.println("Resetting: " + targets.fields());
                    Files.write(tempFile, new Gson().toJson(targets).getBytes());

                    Configuration.config().properties().setProperty("statictracer.rewrite.test_name", minimized.dependentTest());
                    Configuration.config().properties().setProperty("statictracer.rewrite.container", tempFile.toAbsolutePath().toString());

                    final Try<TestRunResult> testRunResult = runner.runList(polluterData.withDeps(minimized.dependentTest()));
                    final TestResult testResult = testRunResult.get().results().get(minimized.dependentTest());

                    System.out.printf("REWRITE_RUN (%s): %s%n",
                            PathManager.modulePath().relativize(RunnerPathManager.outputPath(testRunResult.get())),
                            testResult.result());
                    System.out.printf("NO_REWRITE (%s): %s%n",
                            PathManager.modulePath().relativize(RunnerPathManager.outputPath(minimized.expectedRun())),
                            minimized.expected());
                    System.out.println();

                    return new RewritingResult(targets, testRunResult.get(), testResult.result(), minimized.expected());
                } finally {
                    Files.deleteIfExists(tempFile);
                }

//                Configuration.config().properties().setProperty("statictracer.rewrite.static_field", target.staticFieldName());
//                Configuration.config().properties().setProperty("statictracer.rewrite.field", target.fieldName());
//
//                if (target.field().withoutDepsVal() == null) {
//                    Configuration.config().properties().setProperty("statictracer.rewrite.value", "<null/>");
//                } else {
//                    Configuration.config().properties().setProperty("statictracer.rewrite.value", target.field().withoutDepsVal());
//                }

//                System.out.println("Resetting " + target.fieldName()); // + " to " +
//                        StringUtils.abbreviate(String.valueOf(target.field().withoutDepsVal()), 50));
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
