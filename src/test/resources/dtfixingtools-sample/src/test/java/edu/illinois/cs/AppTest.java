package edu.illinois.cs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class AppTest {
    public static int x = 0;

    @BeforeClass
    public static void beforeClass() throws Exception {
        x = 0;
    }

    @AfterClass
    public static void afterClass() throws Exception {
        x = 0;
    }

    @Test
    public void test1() {
        x = 4;
        assertEquals(4, x);
    }

    @Test
    public void test2() {
        assertEquals(4, x);
    }
}
