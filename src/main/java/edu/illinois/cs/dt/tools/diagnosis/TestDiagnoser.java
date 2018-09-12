package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticFieldInfo;
import edu.illinois.cs.dt.tools.diagnosis.instrumentation.StaticTracer;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutionContainer;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import org.apache.maven.project.MavenProject;

public class TestDiagnoser {
    private final StaticTracer tracer; // The static fields used by the dependent test.
    private final MinimizeTestsResult minimized;

    private final PollutionContainer pollutionContainer;

    private final MavenProject project;
    private final Runner runner;

    public TestDiagnoser(final MavenProject project, final Runner r, final MinimizeTestsResult minimized) {
        this.project = project;
        this.runner = r;
        this.minimized = minimized;

        this.tracer = new StaticFieldInfo(project, minimized).get().getOrDefault(minimized.dependentTest(), new StaticTracer());

        this.pollutionContainer = new PollutionContainer(runner, minimized.deps());
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
