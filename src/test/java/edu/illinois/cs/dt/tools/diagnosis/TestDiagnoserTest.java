package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.subject.SubjectFactory;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.washington.cs.dt.RESULT;
import org.junit.Test;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;

public class TestDiagnoserTest {
    private static final Path JAVA_AGENT = Paths.get("lib/dtdetector-1.2.1.jar").toAbsolutePath();

    private static final String POLLUTED = "com.fake.example.test.TestPollutionDetection.polluted";
    private static final String POLLUTER = "com.fake.example.test.TestPollutionDetection.polluter";

    @Test
    public void testSimple() {
        final MinimizeTestsResult result = new MinimizeTestsResult(RESULT.PASS, POLLUTED, Collections.singletonList(POLLUTER));
        final TestDiagnoser diagnoser =
                new TestDiagnoser(System.getProperty("java.class.path"),
                        JAVA_AGENT, result, SubjectFactory.forPath(Paths.get(".")));

        diagnoser.run();
    }
}