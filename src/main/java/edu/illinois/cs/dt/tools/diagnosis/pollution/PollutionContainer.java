package edu.illinois.cs.dt.tools.diagnosis.pollution;

import java.util.HashMap;
import java.util.Map;

public class PollutionContainer {
    private final String testName;
    private final Map<String, String> withDepsVals;
    private final Map<String, String> withoutDepsVals;

    public PollutionContainer(final String testName, final Map<String, String> withDepsVals, final Map<String, String> withoutDepsVals) {
        this.testName = testName;
        this.withDepsVals = withDepsVals;
        this.withoutDepsVals = withoutDepsVals;
    }

    public Map<String, PollutedField> pollutedFields() {
        final Map<String, PollutedField> pollutedFields = new HashMap<>();

        withoutDepsVals.forEach((k, withoutDepsVal) -> {
            final String withDepsVal = withDepsVals.get(k);
            if (withDepsVal == null || !withoutDepsVal.equals(withDepsVal)) {
                pollutedFields.put(k, new PollutedField(testName, withoutDepsVal, withDepsVal));
            }
        });

        withDepsVals.forEach((k, withDepsVal) -> {
            if (!pollutedFields.containsKey(k)) {
                final String withoutDepsVal = withoutDepsVals.get(k);
                if (withoutDepsVal == null || !withoutDepsVal.equals(withDepsVal)) {
                    pollutedFields.put(k, new PollutedField(testName, withoutDepsVal, withDepsVal));
                }
            }
        });

        return pollutedFields;
    }
}
