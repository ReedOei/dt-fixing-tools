package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestList;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;

public class Diagnoser extends StandardMain {
    public Diagnoser(final String[] args) {
        super(args);
    }

    public static void main(final String[] args) {
        try {
            new Diagnoser(args).run();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void run() throws Exception {
        final String classpath = getArg("cp", "classpath").orElse(System.getProperty("java.class.path"));
        final Path dtFolder = Paths.get(getArgRequired("dtFolder"));
        final Path javaAgent = Paths.get(getArgRequired("javaagent"));

        final Set<MinimizeTestsResult> results = new MinimizeTestList(classpath).runDependentTestFolder(dtFolder);

        for (final MinimizeTestsResult result : results) {
            new TestDiagnoser(classpath, javaAgent, result).run();
        }
    }
}
