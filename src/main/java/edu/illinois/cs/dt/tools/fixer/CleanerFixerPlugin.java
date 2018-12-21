package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.stmt.Statement;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.detection.DetectorPlugin;
import edu.illinois.cs.dt.tools.runner.InstrumentingSmartRunner;
import edu.illinois.cs.dt.tools.utility.ErrorLogger;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.DefaultInvoker;
import org.apache.maven.shared.invoker.InvocationRequest;
import org.apache.maven.shared.invoker.InvocationResult;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import scala.Option;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Stream;

public class CleanerFixerPlugin extends TestPlugin {
    private String classpath;
    private MavenProject project;
    private InstrumentingSmartRunner runner;

    // Don't delete. Need a default constructor for TestPlugin
    public CleanerFixerPlugin() {
    }

    private boolean testOrderPasses(final List<String> tests) {
        return new FailingTestDetector(runner).notPassingTests(tests).orElse(new HashSet<>()).isEmpty();
    }

    private String classpath() throws DependencyResolutionRequiredException {
        final List<String> elements = new ArrayList<>(project.getCompileClasspathElements());
        elements.addAll(project.getRuntimeClasspathElements());
        elements.addAll(project.getTestClasspathElements());

        return String.join(File.pathSeparator, elements);
    }

    @Override
    public void execute(final MavenProject project) {
        this.project = project;

        final Option<Runner> runnerOption = RunnerFactory.from(project);
        final ErrorLogger logger = new ErrorLogger(project);

        System.out.println("DIAGNOSER_MODULE_COORDINATES: " + logger.coordinates());

        logger.runAndLogError(() -> {
            logger.writeSubjectProperties();
            this.classpath = classpath();

            if (runnerOption.isDefined()) {
                this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());

                if (!Files.exists(DetectorPathManager.originalOrderPath())) {
                    Files.write(DetectorPathManager.originalOrderPath(), DetectorPlugin.getOriginalOrder(project));
                }

                // TODO: Modify so that it will read parameters directly from the minimized files
                setupAndApplyFix();
            } else {
                final String errorMsg = "Module is not using a supported test framework (probably not JUnit).";
                TestPluginPlugin.info(errorMsg);
                logger.writeError(errorMsg);
            }

            return null;
        });
    }

    private void setupAndApplyFix() throws Exception {
        // Get all test source files
        final List<Path> testFiles = new ArrayList<>();
        try (final Stream<Path> paths = Files.walk(Paths.get(project.getBuild().getTestSourceDirectory()))) {
            paths.filter(Files::isRegularFile)
                    .forEach(testFiles::add);
        }

        // TODO: Remove these and read from minimized files
        final String polluterTestName = System.getProperty("dt_fixer.polluter", "");
        final String cleanerTestName = System.getProperty("dt_fixer.cleaner");
        final String victimTestName = System.getProperty("dt_fixer.victim");

        final Optional<JavaMethod> cleanerMethodOpt = JavaMethod.find(cleanerTestName, testFiles, classpath);
        final Optional<JavaMethod> victimMethodOpt = JavaMethod.find(victimTestName, testFiles, classpath);

        if (!cleanerMethodOpt.isPresent()) {
            TestPluginPlugin.error("Could not find method " + cleanerTestName);
            TestPluginPlugin.error("Tried looking in: " + testFiles);
            return;
        }

        if (!victimMethodOpt.isPresent()) {
            TestPluginPlugin.error("Could not find method " + victimTestName);
            TestPluginPlugin.error("Tried looking in: " + testFiles);
            return;
        }

        // TODO: applyFix should take in a location for where to output the Java file that contains the
        //       "fixed" code or an option to directly replace the existing test source file.
        applyFix(polluterTestName, cleanerMethodOpt.get(), victimMethodOpt.get());
    }

    private void backup(final JavaFile javaFile) throws IOException {
        final Path path = CleanerPathManager.backupPath(javaFile.path());
        Files.copy(javaFile.path(), path, StandardCopyOption.REPLACE_EXISTING);
    }

    // Try applying cleaner statements and see if test passes when run after polluter
    // Returns true if passes, returns false if not
    private boolean checkCleanerStmts(final String polluterName,
                                      final JavaMethod victimMethod,
                                      final NodeList<Statement> cleanerStmts) throws Exception {
        // Note: this modifies victimMethod, so when we eventually make it delta-debug, we will want
        // to make a copy of the victimMethod somewhere
        victimMethod.prepend(cleanerStmts);
        victimMethod.javaFile().writeAndReloadCompilationUnit();

        // Rebuild and see if tests run properly
        runMvnInstall();
        // TODO: Output to result files rather than stdout
        TestPluginPlugin.info("Running victim test with code from cleaner.");
        List<String> tests;
        if (polluterName.isEmpty()) {
            tests = ListUtil.fromArray(victimMethod.methodName());
        } else {
            tests = ListUtil.fromArray(polluterName, victimMethod.methodName());
        }
        boolean passInIsolationAfterFix = testOrderPasses(tests);
        if (!passInIsolationAfterFix) {
            TestPluginPlugin.error("Fix was unsuccessful. Test still fails with polluter.");
        } else {
            TestPluginPlugin.info("Fix was successful! Fixed file:\n" + victimMethod.javaFile().path());
        }

        // Reset the change
        victimMethod.removeFirstBlock();

        return passInIsolationAfterFix;

        // TODO: Make sure our fix doesn't break any other tests
        //  This block of code could be useful when dealing with a case where we add the necessary
        //  setup to a victim test but our "fix" would actually now cause another test to fail.
        //  We will need some additional logic to deal with this case (i.e., a fix reveals another
        //  dependency) than just this block of code, but it may have its uses later.
        // Check if we pass in the whole test class
        // Should check before fix if class is passing
        //        boolean didClassPass = didTestsPass(victimJavaFile.getTestListAsString());

        //        if (didClassPass) {
        //            boolean didClassPassAfterFix = didTestsPass(victimJavaFile.getTestListAsString());
        //            if (!didClassPassAfterFix) {
        //                System.out.println("Fix was unsuccessful. Fix causes some other test in the class to fail.");
        //                return;
        //            }
        //        }
    }

    // The delta debugging logic to return minimal list of statements to add in to make victim pass
    // In addition to polluter and victim to run, need also list of statements; assumed statements already make victim pass
    // The value n is needed to represent granularity of partitioning the statements to get minimal list
    private NodeList<Statement> deltaDebug(final String polluterName,
                                           final JavaMethod victimMethod,
                                           final NodeList<Statement> cleanerStmts,
                                           int n) throws Exception {
        // If n granularity is greater than number of cleaner statements, then finished, simply return cleaner statements
        if (cleanerStmts.size() < n) {
            return cleanerStmts;
        }

        // Cut the statements into n equal chunks and try each chunk
        int chunkSize = (int)Math.round((double)(cleanerStmts.size()) / n);
        List<NodeList<Statement>> chunks = new ArrayList<>();
        for (int i = 0; i < cleanerStmts.size(); i += chunkSize) {
            NodeList<Statement> chunk = NodeList.nodeList();
            NodeList<Statement> otherChunk = NodeList.nodeList();
            // Start complement chunk by grabbing statements before chunk start
            for (int j = 0; j < i; j++) {
                otherChunk.add(cleanerStmts.get(j));
            }
            // Create chunk starting at this iteration
            for (int j = i; j < Math.min(cleanerStmts.size(), i + chunkSize); j++) {
                chunk.add(cleanerStmts.get(j));
            }
            // Complete complement chunk by grabbing statements after
            for (int j = Math.min(cleanerStmts.size(), i + chunkSize); j < cleanerStmts.size(); j++) {
                otherChunk.add(cleanerStmts.get(j));
            }

            // Check if applying chunk works
            if (checkCleanerStmts(polluterName, victimMethod, chunk)) {
                return deltaDebug(polluterName, victimMethod, chunk, 2);    // If works, then delta debug some more this chunk
            }
            // Otherwise, check if applying complement chunk works
            if (checkCleanerStmts(polluterName, victimMethod, otherChunk)) {
                return deltaDebug(polluterName, victimMethod, otherChunk, n - 1);   // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(polluterName, victimMethod, cleanerStmts, n * 2);
    }

    // TODO: Extract this logic out to a more generalized fixer that is separate, so we can reuse it
    // TODO: for cleaners, santa clauses, etc.
    private void applyFix(final String polluterName,
                          final JavaMethod cleanerMethod,
                          final JavaMethod victimMethod) throws Exception {
        backup(victimMethod.javaFile());

        // Compile the test without our fix
        runMvnInstall();

        // Check if we pass in isolation before fix
        TestPluginPlugin.info("Running victim test with polluter before adding code from cleaner.");
        final List<String> withPolluter =
                // This is to handle the case of a santa clause, where a test will fail with no polluter
                polluterName.isEmpty() ? ListUtil.fromArray(victimMethod.methodName()) :
                                         ListUtil.fromArray(polluterName, victimMethod.methodName());
        if (testOrderPasses(withPolluter)) {
            TestPluginPlugin.error("Test passes with the polluter, but is supposed to fail.");
            return;
        }

        // Do our fix using all cleaner code
        TestPluginPlugin.info("Applying code from cleaner and recompiling.");
        final NodeList<Statement> cleanerStmts = cleanerMethod.body().getStatements();

        if (!checkCleanerStmts(polluterName, victimMethod, cleanerStmts)) {
            TestPluginPlugin.error("Cleaner does not fix victim!");
            return;
        }

        // Cleaner is good, so now we can start delta debugging
        NodeList<Statement> minimalCleanerStmts = deltaDebug(polluterName, victimMethod, cleanerStmts, 2);

        // Apply the final minimal cleaner statements
        victimMethod.prepend(minimalCleanerStmts);
        victimMethod.javaFile().writeAndReloadCompilationUnit();

    }

    private boolean runMvnInstall() throws MavenInvocationException {
        // TODO: Maybe support custom command lines/options?
        final InvocationRequest request = new DefaultInvocationRequest();
        request.setGoals(Arrays.asList("install"));
        request.setPomFile(project.getFile());
        request.setProperties(new Properties());
        request.getProperties().setProperty("skipTests", "true");
        request.getProperties().setProperty("rat.skip", "true");

        // TODO: Log the output from the maven process somewhere
        request.setOutputHandler(s -> {});
        request.setErrorHandler(s -> {});

        final Invoker invoker = new DefaultInvoker();
        final InvocationResult result = invoker.execute(request);

        if (result.getExitCode() != 0) {
            if (result.getExecutionException() == null) {
                throw new RuntimeException("Compilation failed with exit code " + result.getExitCode() + " for an unknown reason");
            } else {
                throw new RuntimeException(result.getExecutionException());
            }
        }

        return true;
    }
}
