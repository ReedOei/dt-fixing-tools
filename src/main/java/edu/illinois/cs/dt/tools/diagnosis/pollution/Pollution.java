package edu.illinois.cs.dt.tools.diagnosis.pollution;

import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldPathManager;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.TracerMode;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.PolluterData;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;

public class Pollution extends FileCache<Map<String, PollutedField>> {
    private final Path path;

    private final Runner runner;
    private final MinimizeTestsResult minimized;
    private final PolluterData polluterData;

    public Pollution(final Runner runner, final MinimizeTestsResult minimized, final PolluterData polluterData) {
        this.runner = runner;
        this.minimized = minimized;

        this.path = PollutionPathManager.pollutionData(minimized);
        this.polluterData = polluterData;
    }

    @Override
    public @NonNull Path path() {
        return path;
    }

    @Override
    protected Map<String, PollutedField> load() {
        // This is actually safe...sorry
        return (Map<String, PollutedField>) TestResult.getXStreamInstance().fromXML(path().toFile());
    }

    @Override
    protected void save() {
        final String s = TestResult.getXStreamInstance().toXML(get());

        try {
            Files.createDirectories(path().getParent());
            Files.write(path(), s.getBytes());
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
    }

    @Override
    protected @NonNull Map<String, PollutedField> generate() {
        try {
            Files.createDirectories(PollutionPathManager.pollutionData());
            StaticFieldPathManager.createModePath(TracerMode.FIRST_ACCESS);

            if (polluterData.deps().isEmpty()) {
                return new HashMap<>();
            }

            final Path withDeps = PollutionPathManager.pollutionData(minimized, "with-deps");
            final Path withoutDeps = PollutionPathManager.pollutionData(minimized, "without-deps");

            // Run with dependencies and monitor first access, then run without and monitor first access
            // If they values of some fields are different, then that's likely the source of the
            // difference in behavior
            System.out.println("Tracking first accesses for: " + minimized.dependentTest());
            StaticTracer.inMode(TracerMode.FIRST_ACCESS, () -> {
                Configuration.config().properties().setProperty("statictracer.first_access.test", minimized.dependentTest());

                runner.runList(polluterData.withDeps(minimized.dependentTest()));
                Files.move(StaticFieldPathManager.infoFor(TracerMode.FIRST_ACCESS, minimized), withDeps);

                runner.runList(Collections.singletonList(minimized.dependentTest()));
                Files.move(StaticFieldPathManager.infoFor(TracerMode.FIRST_ACCESS, minimized), withoutDeps);

                return null;
            }, minimized.hash());

            final Map<String, String> withDepVals = StaticTracer.from(withDeps).firstAccessVals();
            final Map<String, String> withoutDepVals = StaticTracer.from(withoutDeps).firstAccessVals();

            return new PollutionContainer(minimized.dependentTest(), withDepVals, withoutDepVals).pollutedFields();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return new HashMap<>();
    }

    private void forEachFiltered(final BiConsumer<String, PollutedField> consumer) {
        get().forEach((fieldName, diff) -> {
            if (shouldConsume(fieldName)) {
                consumer.accept(fieldName, diff);
            }
        });
    }

    private boolean shouldConsume(final String fieldName) {
        return !fieldName.startsWith("com.thoughtworks.xstream") &&
               !fieldName.startsWith("java.") &&
               !fieldName.startsWith("jdk.") &&
               !fieldName.startsWith("sun.") &&
               !fieldName.startsWith("com.google.gson");
    }

    public Map<String, PollutedField> findPollutions() {
        final Map<String, PollutedField> pollutions = new HashMap<>();

        forEachFiltered((fieldName, field) -> {
            if (field.different()) {
                pollutions.put(fieldName, field);
            }
        });

        return pollutions;
    }
}
