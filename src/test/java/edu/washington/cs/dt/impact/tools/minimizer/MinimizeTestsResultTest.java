package edu.washington.cs.dt.impact.tools.minimizer;

import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MinimizeTestsResultTest {
    @Test
    public void testDisplay() throws Exception {
        final List<String> tests =
                new ArrayList<>(Arrays.asList(
                        "edu.washington.cs.dt.impact.samples.ExampleMinimizeClassA.test2",
                        "edu.washington.cs.dt.impact.samples.ExampleMinimizeClassA.test1",
                        "edu.washington.cs.dt.impact.samples.ExampleMinimizeClassA.test3"
                ));

        final TestMinimizer minimizer =
                new TestMinimizer(tests, "edu.washington.cs.dt.impact.samples.ExampleMinimizeClassA.test3");
        System.out.println(minimizer.run());
    }
}
