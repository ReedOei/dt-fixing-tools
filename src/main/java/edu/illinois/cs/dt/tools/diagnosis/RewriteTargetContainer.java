package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.collections.ListEx;

public class RewriteTargetContainer {
    private final ListEx<RewriteTarget> targets = new ListEx<>();

    public RewriteTargetContainer() {
    }

    public ListEx<RewriteTarget> targets() {
        return targets;
    }

    public void add(final RewriteTarget target) {
        targets.add(target);
    }
}
