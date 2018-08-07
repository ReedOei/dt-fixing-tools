package edu.illinois.cs.dt.tools.runner.data;

import edu.illinois.cs.dt.tools.configuration.Configuration;
import edu.illinois.cs.dt.tools.minimizer.TestMinimizer;
import edu.illinois.cs.dt.tools.runner.SmartTestRunner;
import edu.washington.cs.dt.RESULT;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

public class TestRun {
    private static final int VERIFY_ROUNDS = Configuration.config().getProperty("dt.verify.rounds", 1);

    private final List<String> order;
    private final RESULT result;

    public TestRun(List<String> order, RESULT result) {
        this.order = order;
        this.result = result;
    }

    public List<String> order() {
        return order;
    }

    public RESULT result() {
        return result;
    }

    public boolean verify(final String name, final String classpath) {
        return verify(name, classpath, null);
    }

    public boolean verify(final String dt, final String classpath, final TestMinimizer minimizer) {
        return IntStream.range(0, VERIFY_ROUNDS)
                .allMatch(i -> verifyRound(dt, classpath, minimizer));
    }

    private boolean verifyRound(final String dt, final String classpath, final TestMinimizer minimizer) {
        System.out.printf("[DEBUG] Verifying %s, status: expected %s", dt, this.result);
        RESULT result = null;
        try {
            final List<String> order = new ArrayList<>(this.order);
            if (!order.contains(dt)) {
                order.add(dt);
            }
            result = new SmartTestRunner(classpath).runOrder(order).result().getResult(dt).result;
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
