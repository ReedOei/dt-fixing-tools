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

import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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

                // TODO: Modify so that it will read parameters directly from the
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
        final String polluterTestName = System.getProperty("dt_fixer.polluter");
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

        applyFix(polluterTestName, cleanerMethodOpt.get(), victimMethodOpt.get());
    }

    private void backup(final JavaFile javaFile) throws IOException {
        final Path path = CleanerPathManager.backupPath(javaFile.path());
        Files.copy(javaFile.path(), path, StandardCopyOption.REPLACE_EXISTING);
    }

    // TODO: Extract this logic out to a more generalized fixer that is separate, so we can reuse it
    // TODO: for cleaners, santa clauses, etc.
    private void applyFix(final String polluterName,
                          final JavaMethod cleanerMethod,
                          final JavaMethod victimMethod) throws Exception {
        backup(victimMethod.javaFile());

        // Compile the test without our fix
//        tryRecompile(victimMethod.javaFile());

        runMvnInstall();

        // Check if we pass in isolation before fix
        // TODO: We should get rid of this check eventually, because it won't work for the case of \santaClause
        TestPluginPlugin.info("Running victim test in isolation without code from cleaner.");
        if (!testOrderPasses(Collections.singletonList(victimMethod.methodName()))) {
            TestPluginPlugin.error("Test fails in isolation");
            return;
        }

        // Do our fix
        TestPluginPlugin.info("Applying code from cleaner and recompiling.");
        final NodeList<Statement> cleanerStmts = cleanerMethod.body().getStatements();

        // Note: this modifies victimMethod, so when we eventually make it delta-debug, we will want
        // to make a copy of the victimMethod somewhere
        victimMethod.prepend(cleanerStmts);
        victimMethod.javaFile().writeAndReloadCompilationUnit();

        // Check if we pass in isolation after fix
//        tryRecompile(victimMethod.javaFile());
        runMvnInstall();

        // TODO: Output to result files rather than stdout
        TestPluginPlugin.info("Running victim test in isolation with code from cleaner.");
        boolean passInIsolationAfterFix = testOrderPasses(ListUtil.fromArray(polluterName, victimMethod.methodName()));
        if (!passInIsolationAfterFix) {
            TestPluginPlugin.error("Fix was unsuccessful. Test still fails with polluter.");
        } else {
            TestPluginPlugin.info("Fix was successful! Fixed file:\n" + victimMethod.javaFile().path());
        }

        // TODO: What is this for?
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

    private boolean runMvnInstall() throws MavenInvocationException {
        // TODO: Maybe support custom command lines/options?
        final InvocationRequest request = new DefaultInvocationRequest();
        request.setGoals(Arrays.asList("install"));
        request.setPomFile(project.getFile());
        request.setProperties(new Properties());
        request.getProperties().setProperty("skipTests", "true");

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
