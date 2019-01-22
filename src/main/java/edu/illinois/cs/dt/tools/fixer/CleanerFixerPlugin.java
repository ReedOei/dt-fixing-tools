package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
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
import org.apache.maven.shared.invoker.PrintStreamHandler;
import scala.Option;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
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

    private List<Patch> patches;

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

        this.patches = new ArrayList<>();

        System.out.println("DIAGNOSER_MODULE_COORDINATES: " + logger.coordinates());

        logger.runAndLogError(() -> {
            logger.writeSubjectProperties();
            this.classpath = classpath();

            if (runnerOption.isDefined()) {
                this.runner = InstrumentingSmartRunner.fromRunner(runnerOption.get());

                if (!Files.exists(DetectorPathManager.originalOrderPath())) {
                    Files.write(DetectorPathManager.originalOrderPath(), DetectorPlugin.getOriginalOrder(project));
                }

                // First apply the results from passing orders, fix brittles first
                minimizedResults()
                        .filter(minimized -> minimized.expected().equals(Result.PASS))
                        .collect(Collectors.toList())
                        .forEach(minimized -> {
                            try {
                                setupAndApplyFix(minimized);
                            } catch (Exception e) {
                                e.printStackTrace();
                                throw new RuntimeException(e);
                            }
                        });
                // After fixing brittles, try the polluters (though possible some get fixed due to fixing brittles)
                minimizedResults()
                        .filter(minimized -> !minimized.expected().equals(Result.PASS))
                        .forEach(minimized -> {
                            try {
                                setupAndApplyFix(minimized);
                            } catch (Exception e) {
                                e.printStackTrace();
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
        List<PolluterData> polluterDataOrder = new ArrayList<PolluterData>();

        // If in a passing order and there are multiple potential setters, then prioritize the one in the same test class as dependent test
        if (minimized.expected().equals(Result.PASS)) {
            List<PolluterData> pdWithSameTestClass = new ArrayList<>();
            List<PolluterData> pdWithDiffTestClass = new ArrayList<>();
            for (PolluterData pd : minimized.polluters()) {
                // Only care about case of one polluter
                if (pd.deps().size() == 1) {
                    String setter = pd.deps().get(0);
                    // Want the one in the same test class
                    if (sameTestClass(setter, minimized.dependentTest())) {
                        pdWithSameTestClass.add(pd);
                    } else {
                        pdWithDiffTestClass.add(pd);
                    }
                }
            }
            // Add first in same test class ones, then the remaining ones
            polluterDataOrder.addAll(pdWithSameTestClass);
            polluterDataOrder.addAll(pdWithDiffTestClass);
        } else {
            // If case of failing order with polluters, best bet is one that has a cleaner, and in same test class as victim
            List<PolluterData> pdWithCleaner = new ArrayList<>();
            List<PolluterData> pdWithSingleCleaner = new ArrayList<>();
            List<PolluterData> pdWithSingleCleanerSameTestClassVictim = new ArrayList<>();
            List<PolluterData> pdWithSingleCleanerSameTestClassPolluter = new ArrayList<>();
            for (PolluterData pd : minimized.polluters()) {
                // Consider if has a cleaner
                if (!pd.cleanerData().cleaners().isEmpty()) {
                    pdWithCleaner.add(pd);
                    String polluter = pd.deps().get(0); // TODO: Assuming just one polluter for now...
                    // Would be best to have a cleaner group that is only one test
                    for (CleanerGroup cleanerGroup : pd.cleanerData().cleaners()) {
                        if (cleanerGroup.cleanerTests().size() == 1) {
                            pdWithSingleCleaner.add(pd);
                            // Even more ideal, if the cleaner is in the same test class as victim
                            String cleaner = cleanerGroup.cleanerTests().get(0);
                            if (sameTestClass(cleaner, minimized.dependentTest())) {
                                pdWithSingleCleanerSameTestClassVictim.add(pd);
                            }
                            // Also valid is if in the same test class as the polluter
                            if (sameTestClass(cleaner, polluter)) {
                                pdWithSingleCleanerSameTestClassPolluter.add(pd);
                            }
                        }
                    }
                }
            }
            // Remove from each level duplicates
            pdWithCleaner.removeAll(pdWithSingleCleaner);
            pdWithSingleCleaner.removeAll(pdWithSingleCleanerSameTestClassVictim);
            pdWithSingleCleaner.removeAll(pdWithSingleCleanerSameTestClassPolluter);
            pdWithSingleCleanerSameTestClassVictim.removeAll(pdWithSingleCleanerSameTestClassPolluter);
            // Prioritize based on those levels
            polluterDataOrder.addAll(pdWithSingleCleanerSameTestClassPolluter);
            polluterDataOrder.addAll(pdWithSingleCleanerSameTestClassVictim);
            polluterDataOrder.addAll(pdWithSingleCleaner);
            polluterDataOrder.addAll(pdWithCleaner);
        }

        // Even if could not find way to fix, if there are patches from before, try them to see if they make this one pass
        // TODO: Only really applies to case of polluter/victim
        if (polluterDataOrder.isEmpty() && !patches.isEmpty() && !minimized.expected().equals(Result.PASS)) {
            for (PolluterData pd : minimized.polluters()) {
                List<String> failingOrder = pd.withDeps(minimized.dependentTest());
                JavaMethod victimMethod = JavaMethod.find(minimized.dependentTest(), testFiles, classpath).get();
                if (applyPatchesAndRun(failingOrder, victimMethod)) {
                    TestPluginPlugin.info("Dependent test " + victimMethod.methodName() + " can pass with patches from before.");
                }
            }
        }

        for (PolluterData polluterData : polluterDataOrder) {
            // Apply fix using specific passed in polluter data
            setupAndApplyFix(minimized, polluterData, testFiles);
        }
    }

    private void setupAndApplyFix(final MinimizeTestsResult minimized,
                                  final PolluterData polluterData,
                                  final List<Path> testFiles) throws Exception {
        String polluterTestName;
        Optional<JavaMethod> polluterMethodOpt;
        List<String> failingOrder;

        List<String> cleanerTestNames = new ArrayList<>();  // Can potentially work with many cleaners, try them all

        // If dealing with a case of result with failure, then get standard cleaner logic from it
        if (!minimized.expected().equals(Result.PASS)) {
            polluterTestName = polluterData.deps().get(0);  // Assume only one, get first...
            polluterMethodOpt = JavaMethod.find(polluterTestName, testFiles, classpath);

            if (polluterData.cleanerData().cleaners().isEmpty()) {
                TestPluginPlugin.error("Found polluters for " + minimized.dependentTest() + " but no cleaners.");
                return;
            }

            for (CleanerGroup cleanerGroup : polluterData.cleanerData().cleaners()) {
                // Only handle cleaner groups that have one test each
                if (cleanerGroup.cleanerTests().size() == 1) {
                    cleanerTestNames.add(cleanerGroup.cleanerTests().get(0));   // TODO: Handle cleaner group with more than one test
                }
            }


            // Failing order has both the dependent test and the dependencies
            failingOrder = polluterData.withDeps(minimized.dependentTest());
        } else {
            // "Cleaner" when result is passing is the "polluting" test(s)
            // TODO: Handler group of setters with more than one test
            if (polluterData.deps().size() > 1) {
                TestPluginPlugin.error("There is more than one setter test (currently unsupported)");
                return;
            }
            polluterTestName = null;    // No polluter if minimized order is passing
            polluterMethodOpt = Optional.ofNullable(null);

            cleanerTestNames.add(polluterData.deps().get(0));   // Assume only one, get first...

            // Failing order should be just the dependent test by itself
            failingOrder = Collections.singletonList(minimized.dependentTest());
        }

        String victimTestName = minimized.dependentTest();
        Optional<JavaMethod> victimMethodOpt = JavaMethod.find(victimTestName, testFiles, classpath);

        if (polluterTestName != null && !polluterMethodOpt.isPresent()) {
            TestPluginPlugin.error("Could not find polluter method " + polluterTestName);
            TestPluginPlugin.error("Tried looking in: " + testFiles);
            return;
        }

        if (!victimMethodOpt.isPresent()) {
            TestPluginPlugin.error("Could not find victim method " + victimTestName);
            TestPluginPlugin.error("Tried looking in: " + testFiles);
            return;
        }

        // Give up if cannot find valid cleaner (single test that makes the order pass)
        if (cleanerTestNames.isEmpty()) {
            TestPluginPlugin.error("Could not get a valid cleaner for " + victimTestName);
            return;
        }

        // Try to apply fix with all cleaners, but if one of them works, then we are good
        for (String cleanerTestName : cleanerTestNames) {
            // Reload methods
            victimMethodOpt = JavaMethod.find(victimTestName, testFiles, classpath);
            if (polluterMethodOpt.isPresent()) {
                polluterMethodOpt = JavaMethod.find(victimTestName, testFiles, classpath);
            }
            Optional<JavaMethod> cleanerMethodOpt = JavaMethod.find(cleanerTestName, testFiles, classpath);
            if (!cleanerMethodOpt.isPresent()) {
                TestPluginPlugin.error("Could not find cleaner method " + cleanerTestName);
                TestPluginPlugin.error("Tried looking in: " + testFiles);
                continue;
            }
            // TODO: applyFix should take in a location for where to output the Java file that contains the
            //       "fixed" code or an option to directly replace the existing test source file.
            TestPluginPlugin.info("Applying code from " + cleanerMethodOpt.get().methodName() + " to make " + victimMethodOpt.get().methodName() + " pass.");
            boolean fixSuccess = applyFix(failingOrder, polluterMethodOpt.orElse(null), cleanerMethodOpt.get(), victimMethodOpt.get());
            // A successful patch means we do not need to try all the remaining cleaners for this ordering
            if (fixSuccess) {
                return;
            }
        }
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

    private void restore(final JavaFile javaFile) throws IOException {
        final Path path = CleanerPathManager.backupPath(javaFile.path());
        Files.copy(path, javaFile.path(), StandardCopyOption.REPLACE_EXISTING);
    }

    // Try applying cleaner statements and see if test passes when run after polluter
    // Returns true if passes, returns false if not
    private boolean checkCleanerStmts(final List<String> failingOrder,
                                      final JavaMethod methodToModify,
                                      final NodeList<Statement> cleanerStmts,
                                      boolean prepend, boolean suppressError) throws Exception {
        // If want to prepend set to true, then prepend to victim
        if (prepend) {
            methodToModify.prepend(cleanerStmts);
        } else {
            methodToModify.append(cleanerStmts);
        }
        methodToModify.javaFile().writeAndReloadCompilationUnit();

        // Rebuild and see if tests run properly
        try {
            runMvnInstall(suppressError);
        } catch (Exception ex) {
            TestPluginPlugin.debug("Error building the code, passed in cleaner code does not compile");
            // Reset the change
            if (prepend) {
                methodToModify.removeFirstBlock();
            } else {
                methodToModify.removeLastBlock();
            }
            return false;
        }
        // TODO: Output to result files rather than stdout
        boolean passInFailingOrder = testOrderPasses(failingOrder);

        // Reset the change
        if (prepend) {
            methodToModify.removeFirstBlock();
        } else {
            methodToModify.removeLastBlock();
        }

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

    // The delta debugging logic to return minimal list of statements to add in to make run pass
    // In addition to failing order to run, need also list of statements; assumed statements already make run pass
    // The value n is needed to represent granularity of partitioning the statements to get minimal list
    // The prepend boolean is to decide if to prepend to passed in method or to append
    private NodeList<Statement> deltaDebug(final List<String> failingOrder,
                                           final JavaMethod methodToModify,
                                           final NodeList<Statement> cleanerStmts,
                                           int n, boolean prepend) throws Exception {
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
            if (checkCleanerStmts(failingOrder, methodToModify, chunk, prepend, true)) {
                return deltaDebug(failingOrder, methodToModify, chunk, 2, prepend);         // If works, then delta debug some more this chunk
            }
            // Otherwise, check if applying complement chunk works
            if (checkCleanerStmts(failingOrder, methodToModify, otherChunk, prepend, true)) {
                return deltaDebug(failingOrder, methodToModify, otherChunk, 2, prepend);    // If works, then delta debug some more the complement chunk
            }
        }
        // If not chunk/complement work, increase granularity and try again
        return deltaDebug(failingOrder, methodToModify, cleanerStmts, n * 2, prepend);
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

    private void applyPatch(Patch patch) throws Exception {
        JavaMethod methodToPatch = patch.methodToPatch();
        BlockStmt patchedBlock = patch.patchedBlock();
        if (patch.prepend()) {
            methodToPatch.prepend(patchedBlock.getStatements());
        } else {
            methodToPatch.append(patchedBlock.getStatements());
        }
        methodToPatch.javaFile().writeAndReloadCompilationUnit();
    }

    private boolean applyPatchesAndRun(final List<String> failingOrder, final JavaMethod victimMethod) throws Exception {
        TestPluginPlugin.info("Applying patches from before to see if order still fails.");
        for (Patch patch : patches) {
            TestPluginPlugin.info("Apply patch for " + patch.methodToPatch().methodName());
            applyPatch(patch);
        }
        runMvnInstall(false);
        boolean passWithPatches = testOrderPasses(failingOrder);
        // Regardless, restore all patched files to what they were
        for (Patch patch : patches) {
            if (patch.prepend()) {
                patch.methodToPatch().removeFirstBlock();
            } else {
                patch.methodToPatch().removeLastBlock();
            }
            patch.methodToPatch().javaFile().writeAndReloadCompilationUnit();
            restore(patch.methodToPatch().javaFile());
        }
        runMvnInstall(false);    // Rebuild again, in preparation for next run
        if (passWithPatches) {
            TestPluginPlugin.info("Failing order no longer fails after patches.");
            // If this is a new dependent test and the patches fix it, then save an empty file for it
            // just to help indicate that the test has been fixed
            Path patchFile = CleanerPathManager.fixer().resolve(victimMethod.methodName() + ".patch");
            if (!patchFile.toFile().exists()) {
                Files.write(patchFile, new ArrayList<String>());
            }
            return true;
        }
        return false;
    }

    private JavaMethod addHelperMethod(JavaMethod cleanerMethod, JavaMethod methodToModify, boolean prepend) throws Exception {
        // The modification is to modify the cleaner class to add a helper, then have the other method call the helper
        Expression objectCreation = new ObjectCreationExpr(null, new ClassOrInterfaceType(null, cleanerMethod.getClassName()), NodeList.nodeList());
        Expression helperCall = new MethodCallExpr(objectCreation, "cleanerHelper");
        ExpressionStmt helperCallStmt = new ExpressionStmt(helperCall);
        if (prepend) {
            methodToModify.prepend(NodeList.nodeList(helperCallStmt));
        } else {
            methodToModify.append(NodeList.nodeList(helperCallStmt));
        }
        methodToModify.javaFile().writeAndReloadCompilationUnit();

        String helperName = cleanerMethod.getClassName() + ".cleanerHelper";
        //MethodDeclaration helperMethodDecl = cleanerMethod.javaFile().addMethod(helperName);
        cleanerMethod = JavaMethod.find(cleanerMethod.methodName(), testSources(), classpath).get();    // Reload, just in case
        cleanerMethod.javaFile().addMethod(helperName);
        cleanerMethod.javaFile().writeAndReloadCompilationUnit();
        JavaMethod helperMethod = JavaMethod.find(helperName, testSources(), classpath).get();
        //JavaMethod helperMethod = new JavaMethod(helperName, cleanerMethod.javaFile(), helperMethodDecl);
        //helperMethodDecl.setBody(new BlockStmt());
        helperMethod.javaFile().writeAndReloadCompilationUnit();

        methodToModify = JavaMethod.find(methodToModify.methodName(), testSources(), classpath).get();   // Reload, just in case

        return helperMethod;
    }

    // Returns if applying the fix was successful or not
    private boolean applyFix(final List<String> failingOrder,
                          final JavaMethod polluterMethod,
                          final JavaMethod cleanerMethod,
                          final JavaMethod victimMethod) throws Exception {
        // Check if we pass in isolation before fix
        TestPluginPlugin.info("Running victim test with polluter before adding code from cleaner.");

        if (testOrderPasses(failingOrder)) {
            TestPluginPlugin.error("Failing order doesn't fail.");
            return false;
        }

        // If failing order still failing, apply all the patches from before first to see if already fixed
        if (!patches.isEmpty()) {
            boolean passWithPatch = applyPatchesAndRun(failingOrder, victimMethod);
            if (passWithPatch) {
                return true;
            }
        }

        // Do our fix using all cleaner code, which includes setup and teardown
        TestPluginPlugin.info("Applying code from cleaner and recompiling.");
        final NodeList<Statement> cleanerStmts = NodeList.nodeList();
        // Note: consider both standard imported version (e.g., @Before) and weird non-imported version (e.g., @org.junit.Before)
        // Only include BeforeClass and Before if in separate classes (for both victim and polluter(s))
        //if (!cleanerMethod.getClassName().equals(victimMethod.getClassName())) {
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@BeforeClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.BeforeClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@Before"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.Before"));
        //}
        cleanerStmts.addAll(cleanerMethod.body().getStatements());
        // Only include AfterClass and After if in separate classes (for both victim and polluter(s))
        if (!cleanerMethod.getClassName().equals(victimMethod.getClassName())) {
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@After"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.After"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@AfterClass"));
            cleanerStmts.addAll(getCodeFromAnnotatedMethod(cleanerMethod.javaFile(), "@org.junit.AfterClass"));
        }

        // If polluter/victim case, check if cleaner is same test class as polluter, so append to polluter
        // Method to modify is based on this decision
        boolean prepend = true;
        JavaMethod methodToModify = victimMethod;
        if (polluterMethod != null) {
            if (sameTestClass(cleanerMethod.methodName(), polluterMethod.methodName())) {
                prepend = false;
                methodToModify = polluterMethod;
            }
        }

        // Back up the files we are going to modify
        backup(methodToModify.javaFile());
        backup(cleanerMethod.javaFile());

        // Get the helper method reference
        JavaMethod helperMethod = addHelperMethod(cleanerMethod, methodToModify, prepend);

        // Check if applying these cleaners on the method suffices
        if (!checkCleanerStmts(failingOrder, helperMethod, cleanerStmts, prepend, false)) {
            TestPluginPlugin.error("Applying all of cleaner " + cleanerMethod.methodName() + " to " + methodToModify.methodName() + " does not fix!");
            restore(methodToModify.javaFile());
            restore(helperMethod.javaFile());

            // If does not suffice (and in polluter case), do not abandon hope, try the opposite way as well
            if (polluterMethod != null) {
                if (prepend) {
                    methodToModify = polluterMethod;
                } else {
                    methodToModify = victimMethod;
                }
                backup(methodToModify.javaFile());
                backup(helperMethod.javaFile());
                prepend = !prepend;
                helperMethod = addHelperMethod(cleanerMethod, methodToModify, prepend);
                if (!checkCleanerStmts(failingOrder, helperMethod, cleanerStmts, prepend, false)) {
                    TestPluginPlugin.error("Applying all of cleaner " + cleanerMethod.methodName() + " to " + methodToModify.methodName() + " does not fix!");
                    restore(methodToModify.javaFile());
                    restore(helperMethod.javaFile());
                    return false;
                }
            } else {
                return false;
            }
        }

        // Cleaner is good, so now we can start delta debugging
        final NodeList<Statement> minimalCleanerStmts = deltaDebug(failingOrder, helperMethod, cleanerStmts, 2, prepend);

        // Try to inline these statements into the method
        /*if (prepend) {
            methodToModify.removeFirstBlock();
        } else {
            methodToModify.removeLastBlock();
        }*/
        restore(methodToModify.javaFile());
        methodToModify = JavaMethod.find(methodToModify.methodName(), testSources(), classpath).get();   // Reload, just in case
        if (!checkCleanerStmts(failingOrder, methodToModify, minimalCleanerStmts, prepend, false)) {
            TestPluginPlugin.info("Inlining patch into " + methodToModify.methodName() + " still not good enough to run.");
        }

        // Write out the changes in the form of a patch
        int begin = methodToModify.beginLine() + 1; // Shift one, do not include declaration line
        Path patchFile = CleanerPathManager.fixer().resolve(victimMethod.methodName() + ".patch");  // The patch file is based on the dependent test
        BlockStmt patchedBlock = new BlockStmt(minimalCleanerStmts);
        writePatch(patchFile, begin, patchedBlock, methodToModify.getClassName());
        patches.add(new Patch(methodToModify, patchedBlock, prepend));

        // Report successful patching, report where the patch is
        TestPluginPlugin.info("Patching successful, patch file for " + victimMethod.methodName() + " found at: " + patchFile);

        // Restore the original file
        restore(methodToModify.javaFile());
        restore(helperMethod.javaFile());

        return true;
    }

    // Helper method to create a patch file adding in the passed in block
    private void writePatch(Path patchFile, int begin, BlockStmt blockStmt, String className) throws IOException {
        List<String> patchLines = new ArrayList<>();
        String[] lines = blockStmt.toString().split("\n");
        patchLines.add(className);
        patchLines.add("@@ -" + begin +",0 +" + begin + "," + lines.length + " @@");
        for (String line : lines) {
            patchLines.add("+ " + line);
        }
        Files.createDirectories(patchFile.getParent());
        Files.write(patchFile, patchLines);
    }

    private boolean runMvnInstall(boolean suppressOutput) throws MavenInvocationException {
        // TODO: Maybe support custom command lines/options?
        final InvocationRequest request = new DefaultInvocationRequest();
        request.setGoals(Arrays.asList("install"));
        request.setPomFile(project.getFile());
        request.setProperties(new Properties());
        request.getProperties().setProperty("skipTests", "true");
        request.getProperties().setProperty("rat.skip", "true");
        request.getProperties().setProperty("dependency-check.skip", "true");
        request.getProperties().setProperty("enforcer.skip", "true");

        // TODO: Log the output from the maven process somewhere
        ByteArrayOutputStream baosOutput = new ByteArrayOutputStream();
        PrintStream outputStream = new PrintStream(baosOutput);
        request.setOutputHandler(new PrintStreamHandler(outputStream, true));
        ByteArrayOutputStream baosError = new ByteArrayOutputStream();
        PrintStream errorStream = new PrintStream(baosError);
        request.setErrorHandler(new PrintStreamHandler(errorStream, true));

        final Invoker invoker = new DefaultInvoker();
        final InvocationResult result = invoker.execute(request);

        if (result.getExitCode() != 0) {
            // Print out the contents of the output/error streamed out during evocation, if not suppressed
            if (!suppressOutput) {
                TestPluginPlugin.error(baosOutput.toString());
                TestPluginPlugin.error(baosError.toString());
            }

            if (result.getExecutionException() == null) {
                throw new RuntimeException("Compilation failed with exit code " + result.getExitCode() + " for an unknown reason");
            } else {
                throw new RuntimeException(result.getExecutionException());
            }
        }

        return true;
    }
}
