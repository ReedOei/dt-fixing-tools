package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;
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

                minimizedResults()
                        // TODO: Make sure we don't break the passing order though
                        .forEach(minimized -> {
                            try {
                                setupAndApplyFix(minimized);
                            } catch (Exception e) {
                                throw new RuntimeException(e);
                            }
                        });
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

    private boolean sameTestClass(String test1, String test2) {
        return test1.substring(0, test1.lastIndexOf('.')).equals(test2.substring(0, test2.lastIndexOf('.')));
    }

    private void setupAndApplyFix(final MinimizeTestsResult minimized) throws Exception {
        // Get all test source files
        final List<Path> testFiles = testSources();

        // All minimized orders passed in should have some polluters before (or setters in the case of the order passing)
        if (minimized.polluters().isEmpty()) {
            TestPluginPlugin.error("No polluters for: " + minimized.dependentTest());
            return;
        }

        // TODO: Handle the case where there are multiple polluting/cleaning groups
        // If in a passing order and there are multiple potential setters, then want the one in the same test class as dependent test
        PolluterData polluterData = null;
        if (minimized.expected().equals(Result.PASS)) {
            for (PolluterData pd : minimized.polluters()) {
                // Only care about if case of one polluter
                if (pd.deps().size() == 1) {
                    String setter = pd.deps().get(0);
                    // Want the one in the same test class
                    if (sameTestClass(setter, minimized.dependentTest())) {
                        polluterData = pd;
                        break;
                    }
                }
            }
        } else {
            // If case of failing order with polluters, best bet is one that has a cleaner, and in same test class as victim
            List<PolluterData> pdWithCleaner = new ArrayList<PolluterData>();
            List<PolluterData> pdWithSingleCleaner = new ArrayList<PolluterData>();
            List<PolluterData> pdWithSingleCleanerSameTestClass = new ArrayList<PolluterData>();
            for (PolluterData pd : minimized.polluters()) {
                // Consider if has a cleaner
                if (!pd.cleanerData().cleaners().isEmpty()) {
                    pdWithCleaner.add(pd);
                    // Would be best to have a cleaner group that is only one test
                    for (CleanerGroup cleanerGroup : pd.cleanerData().cleaners()) {
                        if (cleanerGroup.cleanerTests().size() == 1) {
                            pdWithSingleCleaner.add(pd);
                            // Even more ideal, if the cleaner is in the same test class as victim
                            String cleaner = cleanerGroup.cleanerTests().get(0);
                            if (sameTestClass(cleaner, minimized.dependentTest())) {
                                pdWithSingleCleanerSameTestClass.add(pd);
                            }
                        }
                    }
                }
            }
            if (!pdWithSingleCleanerSameTestClass.isEmpty()) {
                polluterData = pdWithSingleCleanerSameTestClass.get(0);
            } else if (!pdWithSingleCleaner.isEmpty()) {
                polluterData = pdWithSingleCleaner.get(0);
            } else if (!pdWithCleaner.isEmpty()) {
                polluterData = pdWithCleaner.get(0);
            }
        }
        // If cannot find any ideal polluter to work with, just take the first
        if (polluterData == null) {
            polluterData = minimized.polluters().get(0);
        }

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
                TestPluginPlugin.error("Cleaner group has more than one test (currently unsupported)");
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
            if (polluterData.deps().size() > 1) {
                TestPluginPlugin.error("There is more than one setter test (currently unsupported)");
                return;
            }
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
        TestPluginPlugin.info("Applying code from " + cleanerMethodOpt.get().methodName() + " into " + victimMethodOpt.get().methodName());
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
            TestPluginPlugin.error("Error building the code, passed in cleaner does not work");
            TestPluginPlugin.error(ex);
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
                return deltaDebug(failingOrder, victimMethod, otherChunk, 2);   // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(failingOrder, victimMethod, cleanerStmts, n * 2);
    }

    private NodeList<Statement> getCodeFromAnnotatedMethod(final JavaFile javaFile, final String annotation) {
        NodeList<Statement> stmts = NodeList.nodeList();
        for (MethodDeclaration method : javaFile.findMethodsWithAnnotation(annotation)) {
            Optional<BlockStmt> body = method.getBody();
            if (body.isPresent()) {
                stmts.addAll(body.get().getStatements());
            }
        }
        return stmts;
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

        // Do our fix using all cleaner code, which includes setup and teardown
        TestPluginPlugin.info("Applying code from cleaner and recompiling.");
        final NodeList<Statement> cleanerStmts = NodeList.nodeList();
        // Note: consider both standard imported version (e.g., @Before) and weird non-imported version (e.g., @org.junit.Before)
        // Only include BeforeClass and Before if in separate classes (for both victim and polluter(s))
        if (!cleanerMethod.getClassName().equals(victimMethod.getClassName())) {
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@BeforeClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.BeforeClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@Before"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.Before"));
        }
        cleanerStmts.addAll(cleanerMethod.body().getStatements());
        // Only include AfterClass and After if in separate classes (for both victim and polluter(s))
        if (!cleanerMethod.getClassName().equals(victimMethod.getClassName())) {
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@After"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.After"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@AfterClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.AfterClass"));
        }

        if (!checkCleanerStmts(failingOrder, victimMethod, cleanerStmts)) {
            TestPluginPlugin.error("Cleaner does not fix victim!");
            return;
        }

        // Cleaner is good, so now we can start delta debugging
        final NodeList<Statement> minimalCleanerStmts = deltaDebug(failingOrder, victimMethod, cleanerStmts, 2);

        // Apply the final minimal cleaner statements
        victimMethod.prepend(minimalCleanerStmts);
        victimMethod.javaFile().writeAndReloadCompilationUnit();

        // Log out what the statements were
        TestPluginPlugin.info("Try patching in this code into the test: " + new BlockStmt(minimalCleanerStmts));
    }

    private boolean runMvnInstall() throws MavenInvocationException {
        // TODO: Maybe support custom command lines/options?
        final InvocationRequest request = new DefaultInvocationRequest();
        request.setGoals(Arrays.asList("install"));
        request.setPomFile(project.getFile());
        request.setProperties(new Properties());
        request.getProperties().setProperty("skipTests", "true");
        request.getProperties().setProperty("rat.skip", "true");
        request.getProperties().setProperty("dependency-check.skip", "true");

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
