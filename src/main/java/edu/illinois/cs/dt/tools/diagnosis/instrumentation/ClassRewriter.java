package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

public class ClassRewriter extends ClassVisitor {
    private String className;

    public ClassRewriter(int api, ClassWriter cv) {
        super(api, cv);
    }

    // Visit the header of the class
    @Override
    public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
        super.visit(version, access, name, signature, superName, interfaces);
        this.className = name;
    }

    @Override
    public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
        MethodVisitor mv = super.visitMethod(access, name, desc, signature, exceptions);
        return new MethodRewriter(api, mv, name, className, desc);  // Instrument method for when it's accessing static fields
    }
}
