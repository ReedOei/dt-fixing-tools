package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import com.google.gson.Gson;
import com.reedoei.eunomia.io.files.FileUtil;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class StaticAccessInfo {
    public static StaticAccessInfo from(final Path path) throws IOException {
        return new Gson().fromJson(FileUtil.readFile(path), StaticAccessInfo.class);
    }

    private final String fieldName;
    private final List<StackTraceElement> stackTrace;

    public StaticAccessInfo(final String fieldName, final StackTraceElement[] stackTrace) {
        // The first 2 are going to be the call the getStackTrace and the call to logStatic, so
        // get rid of them.
        this(fieldName, Arrays.asList(stackTrace).subList(2, stackTrace.length));
    }

    public StaticAccessInfo(final String fieldName, final List<StackTraceElement> stackTrace) {
        this.fieldName = fieldName;
        this.stackTrace = stackTrace;
    }

    @Override
    public String toString() {
        return new Gson().toJson(this);
    }

    public String fieldName() {
        return fieldName;
    }

    public List<StackTraceElement> stackTrace() {
        return stackTrace;
    }
}
