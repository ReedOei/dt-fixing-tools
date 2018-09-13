package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.subject.classpath.Classpath;
import com.reedoei.eunomia.util.ExecutionInfoBuilder;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.testrunner.util.MavenClassLoader;
import edu.illinois.cs.dt.tools.diagnosis.Diagnoser;
import org.apache.maven.project.MavenProject;
import soot.Main;
import soot.Pack;
import soot.PackManager;
import soot.Scene;
import soot.Transform;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Instrumentation extends StandardMain {
    private final String sootCp;
    private final Path inputPath;
    private final Path outputPath;

    public static void instrument(final String sootCp, final Path inputPath, final Path outputPath)
            throws IOException, InterruptedException {
        new ProcessBuilder(new ExecutionInfoBuilder(Instrumentation.class).classpath(sootCp).build().args(
                "--soot-cp", sootCp,
                "--input-dir", inputPath.toString(),
                "--output-dir", outputPath.toString())).inheritIO().start().waitFor();
    }

    public static void instrumentProject(final MavenProject project) throws IOException, InterruptedException {
        if (FileUtil.isEmpty(Paths.get("sootOutput"))) {
            final String sootCp = new MavenClassLoader(project).classpath() + File.pathSeparator +
                    Diagnoser.cp() + File.pathSeparator +
                    Classpath.build(System.getProperty("java.home") + "/lib/*");

            System.out.println("[INFO] Instrumenting test classes.");
            Instrumentation.instrument(sootCp, Paths.get(project.getBuild().getTestOutputDirectory()), StaticFieldInfo.STATIC_FIELD_INFO_PATH);
            System.out.println("[INFO] Instrumenting classes.");
            Instrumentation.instrument(sootCp, Paths.get(project.getBuild().getOutputDirectory()), StaticFieldInfo.STATIC_FIELD_INFO_PATH);
        }
    }

    private Instrumentation(String[] args) {
        super(args);

        this.sootCp = getArg("soot-cp").orElse(System.getProperty("java.class.path"));
        this.inputPath = Paths.get(getArgRequired("input-dir"));
        this.outputPath = Paths.get(getArg("output-dir").orElse("."));
    }

    public static void main(final String[] args) {
        try {
            new Instrumentation(args).run();
        } catch (Exception e) {
            e.printStackTrace();

            System.exit(1);
        }

        System.exit(0);
    }

    @Override
    public void run() throws Exception {
        final Pack jtp = PackManager.v().getPack("jtp");
        jtp.add(new Transform("jtp.instrumenter", new Instrumenter(outputPath)));

        Scene.v().setSootClassPath(sootCp);

        Main.main(new String[] {"-allow-phantom-refs", "-pp", "-w", "-process-path", inputPath.toAbsolutePath().toString()});
        FileUtil.copyFiles(inputPath, Paths.get("sootOutput"));

        jtp.remove("jtp.instrumenter");
    }
}
