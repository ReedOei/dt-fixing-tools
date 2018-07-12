package edu.illinois.cs.dt.tools.runner;

import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.eunomia.util.Util;
import edu.washington.cs.dt.runners.FixedOrderRunner;
import edu.washington.cs.dt.tools.UnitTestFinder;

import java.nio.file.Path;
import java.nio.file.Paths;

public class SimpleRunner extends StandardMain {
    private final Path testsPath;
    private final String classpath;

    private SimpleRunner(final String[] args) {
        super(args);

        testsPath = Paths.get(getArgRequired("tests"));
        classpath = Util.buildClassPath(getArg("cp", "classpath").orElse(System.getProperty("java.class.path")));
    }

    public SimpleRunner(final Path testsPath, final String classpath) {
        super(new String[] {"--tests", testsPath.toString(), "-cp", classpath});

        this.testsPath = testsPath;
        this.classpath = classpath;
    }

    public static void main(final String[] args) {
        try {
            new SimpleRunner(args).run();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.exit(0);
    }

    @Override
    public void run() throws Exception {
        UnitTestFinder.pathOrJarFile = testsPath.toAbsolutePath().toString();

        System.out.println(new FixedOrderRunner(classpath, new UnitTestFinder().findAllTests()).run());
    }
}
