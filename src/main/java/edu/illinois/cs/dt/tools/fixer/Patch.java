package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.stmt.BlockStmt;

public class Patch {
    private JavaMethod methodToPatch;   // Method where patch is applied (prepended or appended)
    private BlockStmt patchedBlock;     // The block to patch into the method
    private boolean prepend;            // Whether to prepend or append to the method

    public Patch(JavaMethod methodToPatch, BlockStmt patchedBlock, boolean prepend) {
        this.methodToPatch = methodToPatch;
        this.patchedBlock = patchedBlock;
        this.prepend = prepend;
    }
}
