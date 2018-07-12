package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.ProcessUtil;
import com.reedoei.eunomia.util.StandardMain;
import com.reedoei.eunomia.util.Util;
import soot.Main;
import soot.Pack;
import soot.PackManager;
import soot.Scene;
import soot.Transform;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Instrumentation extends StandardMain {
    private final String sootCp;
    private final Path inputPath;
    private final Path outputPath;

    public static void instrument(final String sootCp, final Path inputPath, final Path outputPath)
            throws IOException, InterruptedException {
        final Process process = ProcessUtil.runClass(Instrumentation.class,
                "--soot-cp", sootCp,
                "--input-dir", inputPath.toString(),
                "--output-dir", outputPath.toString());
        process.waitFor();
    }

    private Instrumentation(String[] args) {
        super(args);

        this.sootCp = Util.buildClassPath(getArg("soot-cp").orElse(System.getProperty("java.class.path")));
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

        System.out.println("[INFO] Starting Soot");
        Main.main(new String[] {"-allow-phantom-refs", "-pp", "-w", "-process-path", inputPath.toAbsolutePath().toString()});
        FileUtil.copyFiles(inputPath, Paths.get("sootOutput"));

        jtp.remove("jtp.instrumenter");
    }
}
