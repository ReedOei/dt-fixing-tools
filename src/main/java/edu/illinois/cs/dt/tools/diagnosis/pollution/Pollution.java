package edu.illinois.cs.dt.tools.diagnosis.pollution;

import com.google.gson.Gson;
import com.reedoei.eunomia.data.caching.FileCache;
import edu.illinois.cs.dt.tools.diagnosis.DiffContainer;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.illinois.cs.dt.tools.runner.data.TestResult;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;

public class Pollution extends FileCache<Pollution> {
    private final String testName;
    private final Path path;

    private final SmartTestRunner runner;

    private Map<String, DiffContainer.Diff> data = new HashMap<>();

    public Pollution(final SmartTestRunner runner, final Path path) {
        this.path = path;
        this.runner = runner;
        this.testName = path.getFileName().toString();

        load();
    }

    public Pollution(final Path path) {
        this.path = path;
        this.runner = SmartTestRunner.master();
        this.testName = path.getFileName().toString();

        load();
    }

    public Pollution(final String testName) {
        this(SmartTestRunner.master(), testName);
    }

    public Pollution(final SmartTestRunner runner, final String testName) {
        this.runner = runner;
        this.testName = testName;
        this.path = Paths.get("pollution-data").resolve(testName + ".xml");
    }

    @Override
    public @NonNull Path path() {
        return path;
    }

    @Override
    protected Pollution load() {
        data = (Map<String, DiffContainer.Diff>) TestResult.getXStreamInstance().fromXML(path().toFile());

        return this;
    }

    @Override
    protected void save() {
        final String s = TestResult.getXStreamInstance().toXML(data());

        try {
            Files.createDirectories(path().getParent());
            Files.write(path(), s.getBytes());
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
    }

    @Override
    protected Pollution generate() {
        try {
            final Map<String, DiffContainer> diffs = runner.runOrder(Collections.singletonList(testName)).stateDiffs();
            if (diffs.containsKey(testName)) {
                this.data = diffs.get(testName).getDiffs();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return this;
    }

    public Map<String, DiffContainer.Diff> data() {
        return data;
    }

    public String testName() {
        return testName;
    }

    public void forEach(final BiConsumer<String, DiffContainer.Diff> consumer) {
        data.forEach(consumer);
    }

    public void forEachFiltered(final BiConsumer<String, DiffContainer.Diff> consumer) {
        data.forEach((fieldName, diff) -> {
            if (shouldConsume(fieldName, diff)) {
                consumer.accept(fieldName, diff);
            }
        });
    }

    private boolean shouldConsume(final String fieldName, final DiffContainer.Diff diff) {
        if (fieldName.startsWith("com.thoughtworks.xstream") ||
                fieldName.startsWith("java.") ||
                fieldName.startsWith("javax.") ||
                fieldName.startsWith("jdk.") ||
                fieldName.startsWith("sun.")) {
            return false;
        }

        final String[] split = fieldName.split(".");

        if (split.length == 0) {
            return true;
        }

        final String varName = split[split.length - 1];

        // e.g. FIELDS_NAMED_LIKE_THIS shouldn't be considered, because they're probably constants.
        return !varName.toUpperCase().equals(varName);
    }

    public boolean exists() {
        return hasResult();
    }
}
