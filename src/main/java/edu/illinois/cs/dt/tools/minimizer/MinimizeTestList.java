package edu.illinois.cs.dt.tools.minimizer;

import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.eunomia.io.VerbosePrinter;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import edu.illinois.cs.dt.tools.runner.data.DependentTestList;
import org.apache.commons.io.FilenameUtils;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class MinimizeTestList extends StandardMain implements VerbosePrinter {
    private final TestMinimizerBuilder builder;
    private final int verbosity;

    public static void main(final String[] args) {
        try {
            new MinimizeTestList(args).run();
        } catch (Exception e) {
            System.out.println();
            e.printStackTrace();
            System.exit(1);
        }
    }

    public MinimizeTestList() {
        this(new String[0]);
    }

    public MinimizeTestList(final String[] args) {
        super(args);

        final String classpath = getArg("cp", "classpath").orElse(System.getProperty("java.class.path"));
        final Path javaAgent = Paths.get(getArg("javaagent").orElse(""));

        this.verbosity = verbosity(this);

        this.builder = new TestMinimizerBuilder()
                .classpath(classpath)
                .javaAgent(javaAgent)
                .verbosity(verbosity);
    }

    public MinimizeTestList(final String classpath) {
        this(classpath, Paths.get(""));
    }

    public MinimizeTestList(final String classpath, final Path javaAgent) {
        this(new String[] {"-cp", classpath, "--javaagent", javaAgent.toAbsolutePath().toString()});
    }

    private Stream<TestMinimizer> fromDtList(final Path path) {
        println("[INFO] Creating minimizers for file: " + path);

        try {
            return DependentTestList.fromFile(path).dts().stream().flatMap(dt -> dt.minimizers(builder));
        } catch (IOException e) {
            return Stream.empty();
        }
    }

    @Override
    public void run() throws Exception {
        if (getArg("dtFolder").isPresent()) {
            StreamUtil.seq(runDependentTestFolder(Paths.get(getArgRequired("dtFolder"))));
        } else if (getArg("dtFile").isPresent()) {
            StreamUtil.seq(runDependentTestFile(Paths.get(getArgRequired("dtFile"))));
        } else {
            runDefault();
        }
    }

    public Stream<MinimizeTestsResult> runDependentTestFolder(final Path dtFolder) throws IOException {
        return Files.walk(dtFolder)
                .filter(p -> Files.isRegularFile(p))
                .flatMap(this::runDependentTestFile);
    }

    public Stream<MinimizeTestsResult> runDependentTestFile(final Path dtFile) {
        final Optional<Path> outputPath = getArg("outputDir").map(Paths::get);

        return fromDtList(dtFile).flatMap(minimizer -> {
            try {
                final String baseName = FilenameUtils.getBaseName(String.valueOf(dtFile.toAbsolutePath()));
                final Optional<Path> path = outputPath.map(p -> p.resolve(baseName));

                if (path.isPresent()) {
                    try {
                        FileUtil.makeDirectoryDestructive(path.get());
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }

                final MinimizeTestsResult result = minimizer.get();
                path.ifPresent(result::print);
                return Stream.of(result);
            } catch (Exception e) {
                println();
                e.printStackTrace();
            }

            return Stream.empty();
        });
    }

    private void runDefault() throws Exception {
        final Path order = Paths.get(getArgRequired("order"));
        final List<String> testOrder = Files.readAllLines(order, Charset.defaultCharset());
        final String dependentTest = getArg("test").orElse(testOrder.get(testOrder.size() - 1));

        builder.testOrder(testOrder).dependentTest(dependentTest).build().get().print();
    }

    @Override
    public int verbosity() {
        return verbosity;
    }
}
