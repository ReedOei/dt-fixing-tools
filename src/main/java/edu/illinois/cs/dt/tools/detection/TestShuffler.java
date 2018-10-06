package edu.illinois.cs.dt.tools.detection;

import com.google.common.math.IntMath;
import com.google.gson.Gson;
import com.reedoei.eunomia.collections.RandomList;
import com.reedoei.eunomia.io.files.FileUtil;
import com.reedoei.testrunner.configuration.Configuration;
import com.reedoei.testrunner.data.results.TestRunResult;
import edu.illinois.cs.dt.tools.runner.RunnerPathManager;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class TestShuffler {
    private static String className(final String testName) {
        return testName.substring(0, testName.lastIndexOf('.'));
    }

    private final HashMap<String, List<String>> classToMethods;

    private final String type;
    private final List<String> tests;
    private final Set<String> alreadySeenOrders = new HashSet<>();

    public TestShuffler(final String type, final int rounds, final List<String> tests) {
        this.type = type;
        this.tests = tests;

        classToMethods = new HashMap<>();

        for (final String test : tests) {
            final String className = className(test);

            if (!classToMethods.containsKey(className)) {
                classToMethods.put(className, new ArrayList<>());
            }

            classToMethods.get(className).add(test);
        }
    }

    private String historicalType() {
        if (type.equals("random")) {
            return Configuration.config().getProperty("detector.random.historical_type", "random-class");
        } else {

            return Configuration.config().getProperty("detector.random.historical_type", "random");
        }
    }

    // From: https://stackoverflow.com/a/6565597/1498618
    private String md5(final String md5) {
        try {
            final byte[] array = MessageDigest.getInstance("md5").digest(md5.getBytes());

            final StringBuilder sb = new StringBuilder();

            for (final byte anArray : array) {
                sb.append(Integer.toHexString((anArray & 0xFF) | 0x100), 1, 3);
            }

            return sb.toString();
        } catch (NoSuchAlgorithmException ignored) {}
        return "";
    }

    public List<String> shuffledOrder(final int i) {
        final Path historicalRun = DetectorPathManager.detectionRoundPath(historicalType(), i);

        try {
            if (Files.exists(historicalRun)) {
                    return generateHistorical(readHistorical(historicalRun));
            }
        } catch (IOException ignored) {}

        return generateShuffled();
    }

    private List<String> readHistorical(final Path historicalRun) throws IOException {
        final DetectionRound detectionRound = new Gson().fromJson(FileUtil.readFile(historicalRun), DetectionRound.class);

        return detectionRound.testRunIds().stream()
                .flatMap(RunnerPathManager::resultFor)
                .findFirst()
                .map(TestRunResult::testOrder)
                .orElse(new ArrayList<>());
    }

    private List<String> generateHistorical(final List<String> historicalOrder) {
        if ("random-class".equals(type)) {
            return generateWithClassOrder(classOrder(historicalOrder));
        } else {
            return historicalOrder;
        }
    }

    private List<String> generateShuffled() {
        return generateWithClassOrder(new RandomList<>(classToMethods.keySet()).shuffled());
    }

    private List<String> generateWithClassOrder(final List<String> classOrder) {
        final List<String> fullTestOrder = new ArrayList<>();

        for (final String className : classOrder) {
            // random-class only shuffles classes, not methods
            if ("random-class".equals(type)) {
                fullTestOrder.addAll(classToMethods.get(className));
            } else {
                // the standard "random" type, will shuffle both
                fullTestOrder.addAll(new RandomList<>(classToMethods.get(className)).shuffled());
            }
        }

        alreadySeenOrders.add(md5(String.join("", fullTestOrder)));

        return fullTestOrder;
    }

    private List<String> classOrder(final List<String> historicalOrder) {
        return historicalOrder.stream().map(TestShuffler::className).distinct().collect(Collectors.toList());
    }

    @Deprecated
    private int permutations(final int rounds) {
        return permutations(IntMath.factorial(classToMethods.keySet().size()), classToMethods.values().iterator(), rounds);
    }

    @Deprecated
    private int permutations(final int accum, final Iterator<List<String>> iterator, final int rounds) {
        if (accum > rounds) {
            return accum;
        } else {
            if (iterator.hasNext()) {
                final List<String> testsInMethod = iterator.next();

                return permutations(accum * IntMath.factorial(testsInMethod.size()), iterator, rounds);
            } else {
                return accum;
            }
        }
    }
}
