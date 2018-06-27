package edu.washington.cs.dt.impact.samples;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

// DO NOT RUN. These tests are only to be used by other tests (currently only MinimizeTestListTest).
@Ignore
public class ExampleMinimizeClassB {
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
    public void test1() throws Exception {
        x = 4;
        assertEquals(4, x);
    }

    @Test
    public void test2() throws Exception {
        assertEquals(4, x);
    }

    @Test
    public void test3() throws Exception {
        x = 0;
        assertEquals(0, x);
    }

    @Test
    public void test4() throws Exception {
        assertEquals(0, x);
    }
}
