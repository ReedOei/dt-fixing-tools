package edu.illinois.cs.dt.tools.runner.data;

import com.reedoei.eunomia.io.files.FileUtil;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.security.AnyTypePermission;
import edu.illinois.cs.dt.tools.diagnosis.DiffContainer;
import edu.washington.cs.dt.TestExecResult;
import edu.washington.cs.dt.TestExecResults;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class TestResult {
    private final TestExecResults baseResults;
    private final Map<String, DiffContainer> stateDiffs;

    public TestResult(final TestExecResults results) {
        this.baseResults = results;

        Map<String, DiffContainer> stateDiffTemp;
        try {
            stateDiffTemp = readXml();
        } catch (IOException e) {
            stateDiffTemp = new HashMap<>();
        }
        stateDiffs = stateDiffTemp;
    }

    private Map<String,DiffContainer> readXml() throws IOException {
        // TODO: Make this nicer... It really is safe though
        return (Map<String, DiffContainer>) getXStreamInstance().fromXML(FileUtil.readFile(Paths.get("state-diff.xml")));
    }

    private XStream getXStreamInstance() {
        XStream xstream = new XStream(new DomDriver());
        XStream.setupDefaultSecurity(xstream);
        xstream.addPermission(AnyTypePermission.ANY);
        xstream.setMode(XStream.XPATH_ABSOLUTE_REFERENCES);
        // Set fields to be omitted during serialization
        xstream.omitField(java.lang.ref.SoftReference.class, "timestamp");
        xstream.omitField(java.lang.ref.SoftReference.class, "referent");
        xstream.omitField(java.lang.ref.Reference.class, "referent");

        return xstream;
    }

    public TestExecResults results() {
        return baseResults;
    }

    public TestExecResult result() {
        return baseResults.getExecutionRecords().get(0);
    }

    public Map<String, DiffContainer> stateDiffs() {
        return stateDiffs;
    }
}
