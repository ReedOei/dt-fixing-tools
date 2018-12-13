package edu.illinois.cs.dt.tools.diagnosis.rewrite;

import com.reedoei.eunomia.collections.ListEx;

import java.util.Collection;
import java.util.Optional;

public class RewriteTargetContainer {
    private final ListEx<RewriteTarget> targets = new ListEx<>();

    public RewriteTargetContainer() {
    }

    public RewriteTargetContainer(final RewriteTarget target) {
        targets.add(target);
    }

    public RewriteTargetContainer(final Collection<RewriteTarget> targets) {
        this.targets.addAll(targets);
    }

    public ListEx<RewriteTarget> targets() {
        return targets;
    }

    public void add(final RewriteTarget target) {
        targets.add(target);
    }

    public Optional<RewriteTarget> get(final String fieldName) {
        return targets.find(rt -> rt.staticFieldName().equals(fieldName));
    }

    public String fields() {
        return targets().map(RewriteTarget::fieldName).toString();
    }
}
