package edu.illinois.cs.dt.tools.diagnosis;

import com.reedoei.eunomia.collections.ListEx;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RewritingResultContainer {
    private final ListEx<RewritingResult> rewritingResults;

    public RewritingResultContainer(final Stream<RewritingResult> rewritingResultStream) {
        this(rewritingResultStream.collect(Collectors.toList()));
    }

    public RewritingResultContainer(final List<RewritingResult> rewritingResults) {
        this.rewritingResults = new ListEx<>(rewritingResults);
    }

    public ListEx<RewritingResult> rewritingResults() {
        return rewritingResults;
    }

    public ListEx<String> causes() {
        return rewritingResults.filter(r -> !r.result().equals(r.expected())).map(r -> r.target().fieldName());
    }
}
