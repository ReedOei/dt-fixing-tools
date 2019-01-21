package edu.illinois.cs.dt.tools.analysis.data;

public class SameClassPackageCounter {
    private final String testName;
    private int sameClass;
    private int samePackage;
    private int total;

    public SameClassPackageCounter(final String testName) {
        this.testName = testName;
    }

    public boolean anyClassSame() {
        return sameClass > 0;
    }

    public boolean allClassSame() {
        return sameClass == total;
    }

    public boolean anyPackageSame() {
        return samePackage > 0;
    }

    public boolean allPackageSame() {
        return samePackage == total;
    }

    public SameClassPackageCounter count(final String testName) {
        incrementTestClass(testName);
        incrementPackage(testName);

        total++;

        return this;
    }

    private void incrementTestClass(final String testName) {
        if (testClass(this.testName).equals(testClass(testName))) {
            sameClass++;
        }
    }

    private void incrementPackage(final String testName) {
        if (packageName(this.testName).equals(packageName(testName))) {
            samePackage++;
        }
    }

    private String testClass(final String testName) {
        return testName.substring(0, testName.lastIndexOf('.'));
    }

    private String packageName(final String testName) {
        return testClass(testClass(testName));
    }
}
