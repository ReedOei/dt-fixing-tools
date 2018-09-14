package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.lang.instrument.Instrumentation;

public class JavaAgent {

    /**
     * JVM hook to statically load the javaagent at startup.
     * 
     * After the Java Virtual Machine (JVM) has initialized, the premain method
     * will be called. Then the real application main method will be called.
     * 
     * @param args
     * @param inst
     * @throws Exception
     */
    public static void premain(String args, Instrumentation inst) throws Exception {
        Transformer transformer = new Transformer();
        inst.addTransformer(transformer, true);
        inst.retransformClasses(new Class<?>[]{Class.class, ClassLoader.class});
        inst.removeTransformer(transformer);
    }
}
