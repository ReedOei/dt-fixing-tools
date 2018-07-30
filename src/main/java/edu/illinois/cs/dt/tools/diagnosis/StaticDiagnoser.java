package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.subject.Subject;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import soot.PhaseOptions;
import soot.Scene;
import soot.SceneTransformer;
import soot.SootClass;
import soot.jimple.spark.SparkTransformer;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.options.Options;

import java.util.Collections;
import java.util.HashMap;

public class StaticDiagnoser {
    private final Subject subject;
    private final MinimizeTestsResult result;

    public StaticDiagnoser(final Subject subject, final MinimizeTestsResult result) {
        this.subject = subject;
        this.result = result;
    }

    public void diagnose(final String classname) throws Exception {
//        Options.v().setPhaseOption("cg", "enabled:true");
//        Options.v().setPhaseOption("cg.spark", "enabled:true");
//
//        final Pack cg = PackManager.v().getPack("cg");
//        cg.add(new Transform("cg.spark", new CallGraphAnalyzer()));
//
//        final String sootCp = subject.classpath();
//        Scene.v().setSootClassPath(sootCp);
//
//        Main.main(new String[] {"-allow-phantom-refs", "-pp", "-w", "-process-path", subject.testClasses().toAbsolutePath().toString()});
//
//        cg.remove("cg.spark");

        Scene.v().setSootClassPath(System.getProperty("java.class.path"));

        Options.v().set_src_prec(Options.src_prec_java);
        Options.v().set_whole_program(true);
        Options.v().set_allow_phantom_refs(true);
        Options.v().set_process_dir(Collections.singletonList(subject.testClasses().toAbsolutePath().toString()));
//        Options.v().set_time(true);
//        Options.v().set_verbose(true);
//        Options.v().set_debug(true);
//        Options.v().set_debug_resolver(true);
        Scene.v().loadBasicClasses();

        SootClass cl = Scene.v().loadClassAndSupport(classname);
        cl.setApplicationClass();

        Scene.v().loadNecessaryClasses();
        String thePhaseName = "cg";
        HashMap<String, String> theOptions = new HashMap<>();
        theOptions.put("enabled", "true");
        System.out.println("Setting options...");
        SceneTransformer sctform;

//        PhaseOptions.v().setPhaseOption("cg", "implicit-entry:false");
//        PhaseOptions.v().setPhaseOption("cg", "verbose:true");
//        PhaseOptions.v().setPhaseOption("cg", "jdkver:4");
//        PhaseOptions.v().setPhaseOption("cg", "all-reachable:true");

        sctform = SparkTransformer.v();
        sctform.transform(thePhaseName, theOptions);

        System.out.println("Retrieving call graph...");
        CallGraph cg = Scene.v().getCallGraph();

        System.out.println(cg.edgesOutOf(cl.getMethodByName("test2")));

        System.out.println(cg.size());
    }
}
