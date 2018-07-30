package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.subject.SubjectFactory;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.washington.cs.dt.RESULT;
import org.junit.Test;

import java.nio.file.Paths;

import static org.junit.Assert.*;

public class StaticDiagnoserTest {

    public static final String CLASS_A = "edu.illinois.cs.dt.samples.ExampleMinimizeClassA";

    @Test
    public void test() throws Exception {
        final MinimizeTestsResult result =
                new MinimizeTestsResult(RESULT.PASS,
                        CLASS_A + ".test3",
                        ListUtil.fromArray(CLASS_A + ".test2"));

        new StaticDiagnoser(SubjectFactory.forPath(Paths.get(".")), result).diagnose(CLASS_A);
    }
}