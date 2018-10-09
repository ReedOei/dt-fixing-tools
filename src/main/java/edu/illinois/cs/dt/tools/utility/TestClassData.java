package edu.illinois.cs.dt.tools.utility;

import java.util.List;

public class TestClassData {
    public String className;
    public List<String> testNames;

    public TestClassData(String className, List<String> testNames) {
        this.className = className;
        this.testNames = testNames;
    }
}
