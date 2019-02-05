package edu.illinois.cs.dt.tools.fixer;

import java.nio.file.Path;

public class PatchResult {

    private FixStatus status;
    private String dependentTest;
    private String patchLocation;

    public PatchResult(final FixStatus status, final String dependentTest, final String patchLocation) {
        this.status = status;
        this.dependentTest = dependentTest;
        this.patchLocation = patchLocation;
    }

    public FixStatus status() {
        return this.status;
    }

    public String dependentTest() {
        return this.dependentTest;
    }

    public String patchLocation() {
        return this.patchLocation;
    }
}
