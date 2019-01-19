package edu.illinois.cs.dt.tools.runner.data;

import com.google.gson.Gson;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizerBuilder;
import edu.illinois.cs.dt.tools.utility.MD5;

import java.nio.file.Path;
import java.util.Collections;
import java.util.stream.Stream;

public class DependentTest {
    private static final boolean VERIFY_DTS = Configuration.config().getProperty("dt.verify", true);

    private final String name;

    private final TestRun intended;
    private final TestRun revealed;

    public DependentTest(final String name, final TestRun intended, final TestRun revealed) {
        this.name = name;
        this.intended = intended;
        this.revealed = revealed;
    }

    public String name() {
        return name;
    }

    public TestRun intended() {
        return intended;
    }

    public TestRun revealed() {
        return revealed;
    }

    @Override
    public String toString() {
        return new Gson().toJson(this);
    }

    public Stream<TestMinimizer> minimizers(final TestMinimizerBuilder builder, final Runner runner) {
        final TestMinimizerBuilder minimizerBuilder = builder.dependentTest(name);

        if (VERIFY_DTS) {
            if (!intended.verify(name, runner, null) || !revealed.verify(name, runner, null)) {
                return Stream.empty();
            }
        }

        final Result isolationResult = runner.runList(Collections.singletonList(name)).get().results().get(name).result();
        if (!isolationResult.equals(Result.PASS)) { // Does not pass in isolation, needs setter, so need to minimize passing order
            return Stream.of(minimizerBuilder.testOrder(intended.order()).build());
        } else {    // Otherwise passes in isolation, needs polluter, so need to minimize failing order
            return Stream.of(minimizerBuilder.testOrder(revealed.order()).build());
        }
    }

    public boolean verify(final Runner runner, final Path path) {
        return intended.verify(name, runner, path) && revealed.verify(name, runner, path);
    }
}
