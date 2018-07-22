package edu.illinois.cs.dt.tools.utility;

import com.google.common.base.Preconditions;
import com.reedoei.eunomia.data.caching.FileCache;
import com.reedoei.eunomia.subject.Subject;
import com.reedoei.eunomia.util.ProcessUtil;
import com.reedoei.eunomia.util.RuntimeThrower;
import edu.washington.cs.dt.tools.UnitTestFinder;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.io.File;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class TestFinder extends FileCache<List<String>> {
    private final String classpath;
    private final Subject subject;

    public TestFinder(final String classpath, final Subject subject) {
        this.classpath = classpath;
        this.subject = subject;
    }

    public @NonNull Path path() {
        return Paths.get("allunittests.txt");
    }

    @Override
    protected List<String> load() {
        return new RuntimeThrower<>(() -> Files.readAllLines(path(), Charset.defaultCharset())).run();
    }

    @Override
    protected void save() {
        new RuntimeThrower<>(() -> Files.write(path(), String.join(System.lineSeparator(), get()).getBytes())).run();
    }

    @Override
    protected List<String> generate() {
        System.out.println("[INFO] Getting test list.");

        final String cp = classpath + File.pathSeparator + System.getProperty("java.class.path");
        new RuntimeThrower<>(() ->
                ProcessUtil.runClass(cp, UnitTestFinder.class, "--pathOrJarFile", subject.testClasses().toString())
                           .waitFor()).run();
        final List<String> tests = load();
        Preconditions.checkState(!tests.isEmpty(), "No tests found (" + path() + " is empty)");
        return tests;
    }
}
