package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.subject.Subject;
import edu.illinois.cs.dt.tools.diagnosis.pollution.PollutionContainer;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class TestDiagnoser {
    private final Map<String, Set<String>> staticFieldInfo;
    private final Set<String> staticFieldsDt; // The static fields used by the dependent test.
    private final MinimizeTestsResult minimized;

    private final PollutionContainer pollutionContainer;

    private final SmartTestRunner runner;

    public TestDiagnoser(final String classpath, final Path javaAgent,
                         final Map<String, Set<String>> staticFieldInfo, final MinimizeTestsResult minimized,
                         final Subject subject) {
        this.staticFieldInfo = staticFieldInfo;
        this.minimized = minimized;

        this.staticFieldsDt = staticFieldInfo.getOrDefault(minimized.dependentTest(), new HashSet<>());

        runner = new SmartTestRunner(classpath, javaAgent);

        this.pollutionContainer = new PollutionContainer(runner, subject, minimized.deps());
    }

    public PollutionContainer run() {
        if (minimized.deps().isEmpty()) {
            System.out.println("[INFO] No pollutions: No dependencies for " + minimized.dependentTest() + ".");
        } else {
            System.out.println("[INFO] Polluting dts in dependencies of " + minimized.dependentTest());
        }

        return pollutionContainer.with(container -> container.print(staticFieldsDt));
    }
}
