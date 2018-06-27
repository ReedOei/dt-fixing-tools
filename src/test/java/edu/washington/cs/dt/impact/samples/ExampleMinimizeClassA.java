package edu.washington.cs.dt.impact.samples;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import static junit.framework.TestCase.assertEquals;

// DO NOT RUN. These tests are only to be used by other tests (currently only MinimizeTestListTest).
@Ignore
public class ExampleMinimizeClassA {
    private static int x = 0;

    @BeforeClass
    public static void beforeClass() {
        x = 0;
    }

    @AfterClass
    public static void afterClass() {
        x = 0;
    }

    @Test
    public void test1() {
        assertEquals(0, x);
    }

    @Test
    public void test2() {
        x = 4;
        assertEquals(4, x);
    }

    @Test
    public void test3() {
        assertEquals(4, x);
    }
}
