package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.stmt.Statement;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.mavenplugin.TestPlugin;
import com.reedoei.testrunner.mavenplugin.TestPluginPlugin;
import com.reedoei.testrunner.runner.Runner;
import com.reedoei.testrunner.runner.RunnerFactory;
import edu.illinois.cs.dt.tools.detection.DetectorPathManager;
import edu.illinois.cs.dt.tools.detection.DetectorPlugin;
import edu.illinois.cs.dt.tools.minimizer.MinimizeTestsResult;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPathManager;
import edu.illinois.cs.dt.tools.minimizer.MinimizerPlugin;
import edu.illinois.cs.dt.tools.minimizer.PolluterData;
import edu.illinois.cs.dt.tools.minimizer.cleaner.CleanerGroup;
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
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;
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

                Map<String, List<MinimizeTestsResult>> dependentTestResults = minimizedResults()
                        .collect(Collectors.groupingBy(MinimizeTestsResult::dependentTest));
                for (Map.Entry<String, List<MinimizeTestsResult>> entry : dependentTestResults.entrySet()) {
                    setupAndApplyFix(entry.getValue());
                }
            } else {
                final String errorMsg = "Module is not using a supported test framework (probably not JUnit).";
                TestPluginPlugin.info(errorMsg);
                logger.writeError(errorMsg);
            }

            return null;
        });
    }

    private Stream<MinimizeTestsResult> minimizedResults() throws Exception {
        if (Files.exists(MinimizerPathManager.minimized())) {
            return Files.walk(MinimizerPathManager.minimized()).flatMap(p -> {
                try {
                    return Stream.of(MinimizeTestsResult.fromPath(p));
                } catch (IOException ignored) {}

                return Stream.empty();
            });
        } else {
            return detect();
        }
    }

    private Stream<MinimizeTestsResult> detect() throws Exception {
        if (!Files.exists(DetectorPathManager.detectionFile())) {
            if (Configuration.config().getProperty("diagnosis.run_detection", true)) {
                new DetectorPlugin(DetectorPathManager.detectionResults(), runner).execute(project);
            } else {
                throw new NoSuchFileException("File " + DetectorPathManager.detectionFile() + " does not exist and diagnosis.run_detection is set to false");
            }
        }

        return new MinimizerPlugin(runner).runDependentTestFile(DetectorPathManager.detectionFile());
    }

    // Determine which minimized result to try and fix
    private void setupAndApplyFix(final List<MinimizeTestsResult> minimizedResults) throws Exception {
        // Check that there are only two passed in results
        if (minimizedResults.size() != 2) {
            TestPluginPlugin.error("There are " + minimizedResults.size() + " results, should be two");
            return;
        }

        // Go through the results, looking specifically for not passing and has polluters
        for (MinimizeTestsResult minimized : minimizedResults) {
            if (!minimized.expected().equals(Result.PASS)) {
                if (!minimized.polluters().isEmpty()) {
                    setupAndApplyFix(minimized);
                    return;
                }
            }
        }
        // Otherwise, it must require a setter, so iterate again and work with the passing one
        for (MinimizeTestsResult minimized : minimizedResults) {
            if (minimized.expected().equals(Result.PASS)) {
                if (!minimized.polluters().isEmpty()) {
                    setupAndApplyFix(minimized);
                    return;
                }
            }
        }

        // Otherwise, if reached here, then weird case, dunno what to do
        TestPluginPlugin.error("Strange case, not standard case with polluters or setters");
    }

    private void setupAndApplyFix(final MinimizeTestsResult minimized) throws Exception {
        // Get all test source files
        final List<Path> testFiles = testSources();

        // TODO: Handle the case where there are multiple polluting/cleaning groups
        if (minimized.polluters().isEmpty()) {
            TestPluginPlugin.error("No polluters for: " + minimized.dependentTest());
            return;
        }

        final PolluterData polluterData = minimized.polluters().get(0);

        String cleanerTestName;
        Optional<JavaMethod> cleanerMethodOpt;
        List<String> failingOrder;

        // If dealing with a case of result with failure, then get standard cleaner logic from it
        if (!minimized.expected().equals(Result.PASS)) {
            if (polluterData.cleanerData().cleaners().isEmpty()) {
                TestPluginPlugin.error("Found polluters for " + minimized.dependentTest() + " but no cleaners.");
                return;
            }

            final CleanerGroup cleanerGroup = polluterData.cleanerData().cleaners().get(0);

            if (cleanerGroup.cleanerTests().size() > 1) {
                TestPluginPlugin.error("Cleaner groups has more than one test (currently unsupported)");
                return;
            }

            if (cleanerGroup.cleanerTests().isEmpty()) {
                TestPluginPlugin.error("Cleaner group exists but has no tests. This should never happen, and is probably because of a bug in the cleaner finding code.");
                return;
            }
            // TODO: Handle cleaner group with more than one test
            cleanerTestName = cleanerGroup.cleanerTests().get(0);
            cleanerMethodOpt = JavaMethod.find(cleanerTestName, testFiles, classpath);

            // Failing order has both the dependent test and the dependencies
            failingOrder = polluterData.withDeps(minimized.dependentTest());
        } else {
            // "Cleaner" when result is passing is the "polluting" test(s)
            // TODO: Handler group of setters with more than one test
            cleanerTestName = polluterData.deps().get(0);  // Assume only one, get first...
            cleanerMethodOpt = JavaMethod.find(cleanerTestName, testFiles, classpath);

            // Failing order should be just the dependent test by itself
            failingOrder = Collections.singletonList(minimized.dependentTest());
        }

        final String victimTestName = minimized.dependentTest();
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
        applyFix(failingOrder, cleanerMethodOpt.get(), victimMethodOpt.get());
    }

    private List<Path> testSources() throws IOException {
        final List<Path> testFiles = new ArrayList<>();
        try (final Stream<Path> paths = Files.walk(Paths.get(project.getBuild().getTestSourceDirectory()))) {
            paths.filter(Files::isRegularFile)
                    .forEach(testFiles::add);
        }
        return testFiles;
    }

    private void backup(final JavaFile javaFile) throws IOException {
        final Path path = CleanerPathManager.backupPath(javaFile.path());
        Files.copy(javaFile.path(), path, StandardCopyOption.REPLACE_EXISTING);
    }

    // Try applying cleaner statements and see if test passes when run after polluter
    // Returns true if passes, returns false if not
    private boolean checkCleanerStmts(final List<String> failingOrder,
                                      final JavaMethod victimMethod,
                                      final NodeList<Statement> cleanerStmts) throws Exception {
        // Note: this modifies victimMethod, so when we eventually make it delta-debug, we will want
        // to make a copy of the victimMethod somewhere
        victimMethod.prepend(cleanerStmts);
        victimMethod.javaFile().writeAndReloadCompilationUnit();

        // Rebuild and see if tests run properly
        try {
            runMvnInstall();
        } catch (Exception ex) {
            TestPluginPlugin.info("Error building the code, passed in cleaner does not work");
            // Reset the change
            victimMethod.removeFirstBlock();
            return false;
        }
        // TODO: Output to result files rather than stdout
        TestPluginPlugin.info("Running victim test with code from cleaner.");
        boolean passInFailingOrder = testOrderPasses(failingOrder);
        if (!passInFailingOrder) {
            TestPluginPlugin.error("Fix was unsuccessful. Test still fails with polluter.");
        } else {
            TestPluginPlugin.info("Fix was successful! Fixed file:\n" + victimMethod.javaFile().path());
        }

        // Reset the change
        victimMethod.removeFirstBlock();

        return passInFailingOrder;

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
    private NodeList<Statement> deltaDebug(final List<String> failingOrder,
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
            // Create chunk starting at this iteration
            int endpoint = Math.min(cleanerStmts.size(), i + chunkSize);
            chunk.addAll(cleanerStmts.subList(i, endpoint));

            // Complement chunk are elements before and after this current chunk
            otherChunk.addAll(cleanerStmts.subList(0, i));
            otherChunk.addAll(cleanerStmts.subList(endpoint, cleanerStmts.size()));

            // Check if applying chunk works
            if (checkCleanerStmts(failingOrder, victimMethod, chunk)) {
                return deltaDebug(failingOrder, victimMethod, chunk, 2);    // If works, then delta debug some more this chunk
            }
            // Otherwise, check if applying complement chunk works
            if (checkCleanerStmts(failingOrder, victimMethod, otherChunk)) {
                return deltaDebug(failingOrder, victimMethod, otherChunk, n - 1);   // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(failingOrder, victimMethod, cleanerStmts, n * 2);
    }

    // TODO: Extract this logic out to a more generalized fixer that is separate, so we can reuse it
    // TODO: for cleaners, santa clauses, etc.
    private void applyFix(final List<String> failingOrder,
                          final JavaMethod cleanerMethod,
                          final JavaMethod victimMethod) throws Exception {
        backup(victimMethod.javaFile());

        // Check if we pass in isolation before fix
        TestPluginPlugin.info("Running victim test with polluter before adding code from cleaner.");

        if (testOrderPasses(failingOrder)) {
            TestPluginPlugin.error("Failing order doesn't fail.");
            return;
        }

        // Do our fix using all cleaner code
        TestPluginPlugin.info("Applying code from cleaner and recompiling.");
        final NodeList<Statement> cleanerStmts = cleanerMethod.body().getStatements();

        if (!checkCleanerStmts(failingOrder, victimMethod, cleanerStmts)) {
            TestPluginPlugin.error("Cleaner does not fix victim!");
            return;
        }

        // Cleaner is good, so now we can start delta debugging
        final NodeList<Statement> minimalCleanerStmts = deltaDebug(failingOrder, victimMethod, cleanerStmts, 2);

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
