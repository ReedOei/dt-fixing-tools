package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.security.ProtectionDomain;
import java.util.Arrays;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Transformer implements ClassFileTransformer, Opcodes {
    private boolean prefixesMatch(final String[] prefixes, final String className) {
        for (final String prefix : prefixes) {
            if (className.startsWith(prefix)) {
                return true;
            }
        }

        return false;
    }

    @Override
    public byte[] transform(ClassLoader loader, String className, Class classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer)
            throws IllegalClassFormatException {
//        final String[] prefixes = System.getProperty("dtfixingtools.transformer.class_prefix").split(",");
        if (!className.startsWith("sun/") &&
            !className.startsWith("java/") &&
            !className.startsWith("com/sun/") &&
            !className.startsWith("com/thoughtworks/xstream/") &&
            !className.startsWith("javax/xml/") &&
            !className.startsWith("edu/illinois/cs/dt/tools/") &&
            !className.startsWith("org/objectweb/asm/") &&
            !className.startsWith("jdk/")) { // prefixesMatch(prefixes, className)) {
//            System.err.println(className);
            ClassReader cr = new ClassReader(classfileBuffer);          // Read the bytes from the class
            ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS); // Prepare the writer
            ClassRewriter crw = new ClassRewriter(Opcodes.ASM4, cw);    // Modify and write
            cr.accept(crw, 0);                                          // Pass the writer through the reader to rewrite

            // Return the rewritten bytes
            return cw.toByteArray();
        } else {
            return classfileBuffer;
        }
    }
}
