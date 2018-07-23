package edu.illinois.cs.dt.tools.runner.data;

import com.google.common.base.Preconditions;
import com.google.gson.Gson;
import com.reedoei.eunomia.collections.ListUtil;
import com.reedoei.eunomia.io.files.FileUtil;
import edu.washington.cs.dt.RESULT;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DependentTestList {
    public static DependentTestList fromFile(final Path path) throws IOException {
        if (path.getFileName().toString().endsWith(".json")) {
            System.out.println("[INFO] Reading dependent test list from " + path);
            return new Gson().fromJson(FileUtil.readFile(path), DependentTestList.class);
        } else {
            return fromLines(Files.readAllLines(path, Charset.defaultCharset()));
        }
    }

    public static DependentTestList fromLines(final List<String> lines) {
        final List<DependentTest> dts = new ArrayList<>();

        while (lines.size() >= 5) {
            final String testLine = lines.remove(0);
            final String intendedLine = lines.remove(0);
            final String originalOrderLine = lines.remove(0);
            final String revealedLine = lines.remove(0);
            final String modifiedOrderLine = lines.remove(0);

            // Make sure the lines look correct (i.e., start with the right text)
            Preconditions.checkArgument(testLine.startsWith("Test: "));
            Preconditions.checkArgument(intendedLine.startsWith("Intended behavior: "));
            Preconditions.checkArgument(originalOrderLine.startsWith("when executed after: "));
            Preconditions.checkArgument(revealedLine.startsWith("The revealed different behavior: "));
            Preconditions.checkArgument(modifiedOrderLine.startsWith("when executed after: "));

            final String test = testLine.replace("Test: ", "");
            final RESULT intended = RESULT.valueOf(intendedLine.replace("Intended behavior: ", ""));
            final List<String> originalOrder =
                    ListUtil.read(originalOrderLine.replace("when executed after: ", ""));
            final RESULT revealed = RESULT.valueOf(revealedLine.replace("The revealed different behavior: ", ""));
            final List<String> modifiedOrder =
                    ListUtil.read(modifiedOrderLine.replace("when executed after: ", ""));

            dts.add(new DependentTest(test, new TestRun(originalOrder, intended), new TestRun(modifiedOrder, revealed)));
        }

        return new DependentTestList(dts);
    }

    private final List<DependentTest> dts;

    public DependentTestList(final Stream<DependentTest> dts) {
        this(dts.collect(Collectors.toList()));
    }

    public DependentTestList(final List<DependentTest> dts) {
        this.dts = dts;
    }

    @Override
    public String toString() {
        return new Gson().toJson(this);
    }

    public int size() {
        return dts.size();
    }

    public List<String> names() {
        return ListUtil.map(DependentTest::name, dts);
    }

    public List<DependentTest> dts() {
        return dts;
    }
}