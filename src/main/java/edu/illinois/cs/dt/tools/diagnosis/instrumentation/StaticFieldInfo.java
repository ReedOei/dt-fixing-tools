package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.util.RuntimeThrower;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.nio.file.Path;
import java.util.Collections;

public class StaticFieldInfo extends FileCache<StaticTracer> {
    private final Runner runner;
    private final MinimizeTestsResult minimized;

    public StaticFieldInfo(final Runner runner, final MinimizeTestsResult minimized) {
        this.runner = runner;
        this.minimized = minimized;
    }

    @Override
    public @NonNull Path path() {
        return StaticFieldPathManager.infoFor(TracerMode.TRACK, minimized);
    }

    @Override
    protected StaticTracer load() {
        return new RuntimeThrower<>(() -> StaticTracer.from(path())).run();
    }

    @Override
    protected void save() {
        // Intentionally left empty. It will automatically save on generation.
    }

    @Override
    protected @NonNull StaticTracer generate() {
        return new RuntimeThrower<>(() -> {
            generateStaticFieldInfo();
            return load();
        }).run();
    }

    private void generateStaticFieldInfo() throws Exception {
        StaticFieldPathManager.createModePath(TracerMode.TRACK);

        System.out.println("Tracking static field access for: " + minimized.dependentTest());

        StaticTracer.inMode(TracerMode.TRACK, () -> {
            runner.runList(Collections.singletonList(minimized.dependentTest()));

            return null;
        }, minimized.hash());
    }
}
