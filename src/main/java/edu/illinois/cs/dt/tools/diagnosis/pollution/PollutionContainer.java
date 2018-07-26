package edu.illinois.cs.dt.tools.diagnosis.pollution;

import com.github.javaparser.utils.StringEscapeUtils;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.subject.Subject;
import edu.illinois.cs.dt.tools.diagnosis.DiffContainer;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.codehaus.plexus.util.StringUtils;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;

public class PollutionContainer extends FileCache<PollutionContainer> {
    private final SmartTestRunner runner;
    private final Subject subject;
    private final List<String> tests;

    private Map<String, Pollution> data = new HashMap<>();

    public PollutionContainer(final SmartTestRunner runner, final Subject subject, final List<String> tests) {
        this.runner = runner;
        this.subject = subject;
        this.tests = tests;
    }

    @Override
    protected boolean hasResult() {
        return tests.stream().allMatch(testName -> new Pollution(runner, testName).exists());
    }

    @Override
    public @NonNull Path path() {
        return Paths.get("pollution-data");
    }

    @Override
    protected PollutionContainer load() {
        tests.forEach(testName -> data.put(testName, new Pollution(runner, testName).load()));

        return this;
    }

    @Override
    protected void save() {
        forEach((testName, pollution) -> pollution.save());
    }

    @Override
    protected PollutionContainer generate() {
        System.out.println("[INFO] Generating pollution data");
        tests.forEach(testName -> data.put(testName, new Pollution(runner, testName).get()));
        return this;
    }

    public void forEach(final BiConsumer<String, Pollution> consumer) {
        data.forEach(consumer);
    }

    private String formatDiff(final String fieldName, final DiffContainer.Diff diff) {
        return String.format("%s: (%s, %s)", fieldName,
                StringEscapeUtils.escapeJava(StringUtils.abbreviate(String.valueOf(diff.getBefore()), 30)),
                StringEscapeUtils.escapeJava(StringUtils.abbreviate(String.valueOf(diff.getAfter()), 30)));
    }

    public void print(final Set<String> staticFieldsDt) {
        forEach((testName, pollution) -> {
            if (!pollution.data().isEmpty()) {
                System.out.println("[INFO] Found pollution from: " + testName);

                pollution.forEach((fieldName, diff) -> {
                    if (staticFieldsDt.contains(fieldName)) {
                        System.out.println("-----------------------------------------------------");
                        System.out.println("**Accessed by test** " + formatDiff(fieldName, diff));
                        System.out.println("-----------------------------------------------------");
                    } else {
                        System.out.println(formatDiff(fieldName, diff));
                    }
                });

                System.out.println();
            }
        });
    }
}
