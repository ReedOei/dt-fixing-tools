package edu.illinois.cs.dt.tools.minimizer;

import com.google.common.base.Preconditions;
import com.google.common.collect.Streams;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.collections.StreamUtil;
import com.reedoei.eunomia.io.VerbosePrinter;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.eunomia.util.StandardMain;
import edu.washington.cs.dt.RESULT;
import org.apache.commons.io.FilenameUtils;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
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

        final List<String> lines;
        try {
            lines = Files.readAllLines(path, Charset.defaultCharset());
        } catch (IOException e) {
            e.printStackTrace();
            return Stream.empty();
        }
        lines.removeIf(s -> s.isEmpty() || s.trim().isEmpty());

        return Streams.stream(new TestMinimizerIterator(lines));
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

                final MinimizeTestsResult result = minimizer.run();
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

        builder.testOrder(testOrder).dependentTest(dependentTest).build().run().print();
    }

    @Override
    public int verbosity() {
        return verbosity;
    }

    private class TestMinimizerIterator implements Iterator<TestMinimizer> {
        private final List<TestMinimizer> nextMinimizers = new ArrayList<>();
        private final List<String> lines;

        public TestMinimizerIterator(List<String> lines) {
            this.lines = lines;
        }

        @Override
        public boolean hasNext() {
            return !nextMinimizers.isEmpty() || lines.size() >= 5;
        }

        @Override
        public TestMinimizer next() {
            if (!nextMinimizers.isEmpty()) {
                return nextMinimizers.remove(0);
            }

            // Verify there are enough lines to contain all the information we need.
            Preconditions.checkArgument(lines.size() >= 5);

            final String testLine = lines.remove(0);
            final String intendedLine = lines.remove(0);
            final String originalOrderLine = lines.remove(0);
            final String revealedLine = lines.remove(0);
            final String modifiedOrderLine = lines.remove(0);

            // Make sure the lines look correct (i.e., start with the right text)
            Preconditions.checkArgument(testLine.startsWith("Test: "));
            Preconditions.checkArgument(intendedLine.startsWith("Intended behavior: "));
            Preconditions.checkArgument(originalOrderLine.startsWith("when executed after: "));
            Preconditions.checkArgument(revealedLine.startsWith("The revealed different behavior: "));
            Preconditions.checkArgument(modifiedOrderLine.startsWith("when executed after: "));

            final String test = testLine.replace("Test: ", "");
            final RESULT intended = RESULT.valueOf(intendedLine.replace("Intended behavior: ", ""));
            final List<String> originalOrder =
                    ListUtil.read(originalOrderLine.replace("when executed after: ", ""));
            final RESULT revealed = RESULT.valueOf(revealedLine.replace("The revealed different behavior: ", ""));
            final List<String> modifiedOrder =
                    ListUtil.read(modifiedOrderLine.replace("when executed after: ", ""));

            final TestMinimizerBuilder orderBuilder = builder.dependentTest(test);

            try {
                // If they're the same, don't both creating two
                if (intended != revealed) {
                    nextMinimizers.add(orderBuilder.testOrder(originalOrder).build());
                }

                nextMinimizers.add(orderBuilder.testOrder(modifiedOrder).build());
            } catch (Exception e) {
                e.printStackTrace();
            }

            if (!nextMinimizers.isEmpty()) {
                return nextMinimizers.remove(0);
            } else {
                return null;
            }
        }
    }
}
