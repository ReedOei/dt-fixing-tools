package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.subject.Subject;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutionContainer;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;

import java.nio.file.Path;

public class TestDiagnoser {
    private final StaticTracer tracer; // The static fields used by the dependent test.
    private final MinimizeTestsResult minimized;

    private final PollutionContainer pollutionContainer;

    private final SmartTestRunner runner;

    public TestDiagnoser(final String classpath, final Path javaAgent,
                         final MinimizeTestsResult minimized,
                         final Subject subject) {
        this.minimized = minimized;

        this.tracer = new StaticFieldInfo(subject, minimized).get().getOrDefault(minimized.dependentTest(), new StaticTracer());

        runner = new SmartTestRunner(classpath, javaAgent);

        this.pollutionContainer = new PollutionContainer(runner, subject, minimized.deps());
    }

    public PollutionContainer run() {
        if (minimized.deps().isEmpty()) {
            System.out.println("[INFO] No pollutions: No dependencies for " + minimized.dependentTest() + ".");
        } else {
            System.out.println("[INFO] Polluting dts in dependencies of " + minimized.dependentTest());
        }

        return pollutionContainer.with(container -> container.print(tracer));
    }
}
