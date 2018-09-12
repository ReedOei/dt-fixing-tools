package edu.illinois.cs.dt.tools.runner.data;

import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.Result;
import com.reedoei.testrunner.runner.Runner;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

public class TestRun {
    private static final int VERIFY_ROUNDS = Configuration.config().getProperty("dt.verify.rounds", 1);

    private final List<String> order;
    private final Result result;

    public TestRun(final List<String> order, final Result result) {
        this.order = order;
        this.result = result;
    }

    public List<String> order() {
        return order;
    }

    public Result result() {
        return result;
    }

    public boolean verify(final String name, final Runner runner) {
        return verify(name, runner, null);
    }

    public boolean verify(final String dt, final Runner runner, final TestMinimizer minimizer) {
        return IntStream.range(0, VERIFY_ROUNDS)
                .allMatch(i -> verifyRound(dt, runner, minimizer));
    }

    private boolean verifyRound(final String dt, final Runner runner, final TestMinimizer minimizer) {
        System.out.printf("[DEBUG] Verifying %s, status: expected %s", dt, this.result);
        Result result = null;
        try {
            final List<String> order = new ArrayList<>(this.order);
            if (!order.contains(dt)) {
                order.add(dt);
            }
            result = runner.runList(order).get().results().get(dt).result();
        } catch (Exception ignored) {}

        if (minimizer != null) {
            System.out.printf(", got %s, minimizer got %s\n", result, minimizer.expected());
            return this.result.equals(result) && this.result.equals(minimizer.expected());
        } else {
            System.out.printf(", got %s\n", result);
            return this.result.equals(result);
        }
    }
}
