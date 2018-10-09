package edu.illinois.cs.dt.tools.utility;

import com.reedoei.eunomia.util.StandardMain;

import junit.framework.Test;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collectors;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


/**
 * Created by winglam on 10/8/18.
 */
public class GetMavenTestOrder extends StandardMain {

    @Override protected void run() throws Exception {
        final List<String> classOrder = getClassOrder(mvnTestLog.toFile());

        final List<Path> allResultsFolders = Files.walk(sureFireDirectory)
                                                     .filter(path -> path.toString().contains("TEST-"))
                                                     .collect(Collectors.toList());

        TreeMap<Long, List<TestClassData>> timeToTestClass = new TreeMap<>();
        for (int i = 0; i < allResultsFolders.size(); i++) {
            final Path p = allResultsFolders.get(i);

            File f = p.toFile();
            long time = f.lastModified();

            List<TestClassData> currentList = timeToTestClass.get(time);
            if (currentList == null) {
                currentList = new ArrayList<>();
            }
            currentList.add(parseXML(f));

            timeToTestClass.put(time, currentList);
        }

        StringBuilder sb = new StringBuilder();
        for (Long time : timeToTestClass.keySet()) {
            sb.append(time);
            sb.append(" :\n");
            List<TestClassData> dataList = timeToTestClass.get(time);

            if (dataList.size() > 1) {

                TreeMap<Integer, List<TestClassData>> indexToTestClass = new TreeMap<>();
                for (TestClassData data : dataList) {
                    int index = classOrder.indexOf(data.className);
                    List<TestClassData> currentList = indexToTestClass.get(index);
                    if (currentList == null) {
                        currentList = new ArrayList<>();
                    }
                    currentList.add(data);

                    indexToTestClass.put(index, currentList);
                }

                for (Integer index : indexToTestClass.keySet()) {
                    List<TestClassData> indexList = indexToTestClass.get(index);
                    if (indexList.size() > 1) {
                        for (TestClassData data : indexList) {
                            setStringBuilderTestClassData(data, sb);
                        }
                    } else {
                        setStringBuilderTestClassData(indexList.get(0), sb);
                    }
                }

            } else {
                setStringBuilderTestClassData(dataList.get(0), sb);
            }
        }
    }

    private void setStringBuilderTestClassData(TestClassData data, StringBuilder sb) {
        sb.append("  ");
        sb.append(data.className);
        sb.append(" : ");
        sb.append(data.testNames);
        System.out.println(sb.toString());
        sb.setLength(0);
    }

    private List<String> getClassOrder(File f) {
        List<String> classNames = new ArrayList<>();
        try {
            FileReader fileReader = new FileReader(f);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                if (line.trim().startsWith("Running ")) {
                    String className = line.trim().split(" ")[1];
                    classNames.add(className);
                }
            }
            fileReader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return classNames;
    }

    private class TestClassData {
        public String className;
        public List<String> testNames;

        public TestClassData(String className, List<String> testNames) {
            this.className = className;
            this.testNames = testNames;
        }
    }

    private TestClassData parseXML(File xmlFile) {
        List<String> testNames = new ArrayList<>();
        String className = "";

        try {
            DocumentBuilder dBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document doc = dBuilder.parse(xmlFile);

            Element rootElement = doc.getDocumentElement();
            rootElement.normalize();

            int errors = Integer.parseInt(rootElement.getAttribute("errors"));
            int failures = Integer.parseInt(rootElement.getAttribute("failures"));

            if (errors != 0 || failures != 0) {
                // errors/failures found in the test suite from running mvn test.
                // this test suite should not proceed to use detectors
                // TODO

            }

            className = rootElement.getAttribute("name");

            NodeList nList = doc.getElementsByTagName("testcase");
            for (int temp = 0; temp < nList.getLength(); temp++) {
                Node nNode = nList.item(temp);

                if (nNode.getNodeType() == Node.ELEMENT_NODE) {

                    Element eElement = (Element) nNode;

                    if (eElement.getElementsByTagName("skipped").getLength() != 0) {
                        // this test case was marked as skip and therefore should not be ran by us
                        // TODO
                        continue;
                    }

                    String testName = eElement.getAttribute("name");
                    testNames.add(testName);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return new TestClassData(className, testNames);
    }

    private final Path mvnTestLog;
    private final Path sureFireDirectory;

    private GetMavenTestOrder(final String[] args) {
        super(args);

        this.sureFireDirectory = Paths.get(getArgRequired("sureFireDirectory")).toAbsolutePath();
        this.mvnTestLog = Paths.get(getArgRequired("mvnTestLog")).toAbsolutePath();
    }

    public static void main(final String[] args) {
        try {
            new GetMavenTestOrder(args).run();

            System.exit(0);
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.exit(1);
    }
}
