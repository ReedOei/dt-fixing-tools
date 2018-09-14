package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import java.util.Arrays;
import java.util.HashSet;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.ClassVisitor;

public class MethodRewriter extends MethodVisitor {

    String methodName;
    String className;
    String methodDesc;

    public MethodRewriter(int api, MethodVisitor mv, String methodName, String className, String desc) {
        super(api, mv);
        this.methodName = methodName;
        this.className = className;
        this.methodDesc = desc;
    }

    @Override
    public void visitFieldInsn(int opcode, String owner, String name, String desc) {
        super.visitFieldInsn(opcode, owner, name, desc);
        // Instrument if it's GETSTATIC, adding code after the access
        if (opcode == Opcodes.GETSTATIC) {
            super.visitLdcInsn(owner.replace('/', '.') + "." + name);   // Change to make the field name have .
            super.visitMethodInsn(Opcodes.INVOKESTATIC, "edu/illinois/cs/dt/tools/diagnosis/instrumentation/StaticTracer", "logStatic", "(Ljava/lang/String;)V");
        }
    }
}
