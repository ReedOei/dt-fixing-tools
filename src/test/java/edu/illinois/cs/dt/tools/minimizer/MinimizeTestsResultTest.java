package edu.illinois.cs.dt.tools.minimizer;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MinimizeTestsResultTest {
    @Test
    public void testDisplay() throws Exception {
        final List<String> tests =
                new ArrayList<>(Arrays.asList(
                        "edu.illinois.cs.dt.samples.ExampleMinimizeClassA.test2",
                        "edu.illinois.cs.dt.samples.ExampleMinimizeClassA.test1",
                        "edu.illinois.cs.dt.samples.ExampleMinimizeClassA.test3"
                ));

        final TestMinimizer minimizer =
                new TestMinimizerBuilder().testOrder(tests).dependentTest("edu.illinois.cs.dt.samples.ExampleMinimizeClassA.test3").build();
        System.out.println(minimizer.get());
    }
}
