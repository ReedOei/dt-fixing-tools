package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.subject.Subject;
import com.reedoei.eunomia.subject.SubjectFactory;
import com.reedoei.eunomia.util.Util;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import org.apache.commons.io.FileUtils;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;

import static org.junit.Assume.assumeTrue;

public class DiagnoserTest {
    private static final Path JAVA_AGENT = Paths.get("lib/dtdetector-1.2.1-SNAPSHOT.jar").toAbsolutePath();
    private static final Path TEST_PROJECT_PATH = Paths.get("src/test/resources/dtfixingtools-sample").toAbsolutePath();

    @Before
    public void setUp() throws Exception {
        cleanup();
    }

    @After
    public void tearDown() throws Exception {
//        cleanup();
    }

    private void cleanup() throws Exception {
        FileUtils.deleteQuietly(Paths.get("allunittests.txt").toFile());
        FileUtils.deleteDirectory(Paths.get("minimized").toFile());
        FileUtils.deleteDirectory(Paths.get("detection-results").toFile());
        FileUtils.deleteDirectory(Paths.get("pollution-data").toFile());
//        FileUtils.deleteDirectory(Paths.get("sootOutput").toFile());
        FileUtils.deleteDirectory(StaticFieldInfo.STATIC_FIELD_INFO_PATH.toFile());
    }

    @Test
    public void diagnose() throws Exception {
        assumeTrue(Files.exists(JAVA_AGENT));
        assumeTrue(Files.exists(TEST_PROJECT_PATH));

        final @NonNull Subject subject = SubjectFactory.forPath(TEST_PROJECT_PATH);

        final String cp = subject.classpath() + File.pathSeparator
                + System.getProperty("java.class.path") + File.pathSeparator
                + Util.buildClassPath("lib/*");

        final Diagnoser diagnoser = new Diagnoser(subject, cp, JAVA_AGENT);

        diagnoser.diagnose().collect(Collectors.toList());
    }
}