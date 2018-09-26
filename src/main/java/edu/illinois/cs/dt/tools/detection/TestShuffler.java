package edu.illinois.cs.dt.tools.detection;

import com.google.common.math.IntMath;
import com.reedoei.eunomia.collections.RandomList;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class TestShuffler {
    private final HashMap<String, List<String>> classToMethods;

    private final int permutationCount;

    public TestShuffler(final int rounds, final List<String> tests) {
        classToMethods = new HashMap<>();

        for (final String test : tests) {
            final String className = test.substring(0, test.lastIndexOf('.'));

            if (!classToMethods.containsKey(className)) {
                classToMethods.put(className, new ArrayList<>());
            }

            classToMethods.get(className).add(test);
        }

        permutationCount = permutations(rounds);
    }

    // From: https://stackoverflow.com/a/6565597/1498618
    private String MD5(String md5) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("MD5");
            byte[] array = md.digest(md5.getBytes());
            final StringBuilder sb = new StringBuilder();
            for (final byte anArray : array) {
                sb.append(Integer.toHexString((anArray & 0xFF) | 0x100).substring(1, 3));
            }
            return sb.toString();
        } catch (java.security.NoSuchAlgorithmException ignored) {}
        return "";
    }

    public List<String> shuffledOrder(final Set<String> alreadySeenOrders) {
        while (true) {
            @NonNull final RandomList<String> classOrder = new RandomList<>(classToMethods.keySet()).shuffled();

            final List<String> fullTestOrder = new ArrayList<>();

            for (final String className : classOrder) {
                fullTestOrder.addAll(new RandomList<>(classToMethods.get(className)).shuffled());
            }

            final String hash = MD5(String.join("", fullTestOrder));

            if (!alreadySeenOrders.contains(hash)) {
                alreadySeenOrders.add(hash);

                return fullTestOrder;
            } else {
                System.out.println("Skipping duplicate order");
            }
        }
    }

    private int permutations(final int rounds) {
        return permutations(IntMath.factorial(classToMethods.keySet().size()), classToMethods.values().iterator(), rounds);
    }

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

    public int permutationCount() {
        return permutationCount;
    }
}
