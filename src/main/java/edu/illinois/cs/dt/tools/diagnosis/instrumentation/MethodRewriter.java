package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class MethodRewriter extends MethodVisitor {
    private static final String STATIC_TRACER_CLASS_NAME = StaticTracer.class.getCanonicalName().replace(".", "/");
    private static final String SYSTEM_CLASS_NAME = System.class.getCanonicalName().replace(".", "/");

    private String methodName;
    private String className;
    private String methodDesc;

    public MethodRewriter(int api, MethodVisitor mv, String methodName, String className, String desc) {
        super(api, mv);
        this.methodName = methodName;
        this.className = className;
        this.methodDesc = desc;
    }

    @Override
    public void visitEnd() {
        super.visitEnd();
    }

    @Override
    public AnnotationVisitor visitAnnotation(final String desc, final boolean visible) {
//        System.out.println(desc);
        return super.visitAnnotation(desc, visible);
    }

    @Override
    public void visitFieldInsn(int opcode, String owner, String name, String desc) {
        // Instrument if it's getting/setting a static variable, adding code before access.
        // Important this comes BEFORE the access, otherwise rewriting (TracerMode.REWRITE),
        // would not work (it would change the value AFTER it is first used)
        if (opcode == Opcodes.GETSTATIC) {
            super.visitLdcInsn(owner.replace('/', '.') + "." + name);   // Change to make the field name have .
            super.visitMethodInsn(Opcodes.INVOKESTATIC,
                    STATIC_TRACER_CLASS_NAME,
                    "logStatic",
                    "(Ljava/lang/String;)V",
                    false);
        }
        super.visitFieldInsn(opcode, owner, name, desc);
    }

    @Override
    public void visitMethodInsn(final int opcode, final String owner, final String name, final String desc, final boolean itf) {
        if (opcode == Opcodes.INVOKESTATIC) {
            if ("getProperty".equals(name)) {
                if (SYSTEM_CLASS_NAME.equals(owner)) {
                    // There are two versions of this method:
                    // One takes two arguments (key and default), the other only takes the key
                    if (desc.equals("(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;")) {
                        super.visitInsn(Opcodes.SWAP);
                        super.visitInsn(Opcodes.DUP);
                        super.visitLdcInsn("System.property.");   // Change to make the field name have System.property in front of it.
                        super.visitInsn(Opcodes.SWAP);

                        super.visitMethodInsn(Opcodes.INVOKESTATIC,
                                STATIC_TRACER_CLASS_NAME,
                                "concat",
                                "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;",
                                false);

                        super.visitMethodInsn(Opcodes.INVOKESTATIC,
                                STATIC_TRACER_CLASS_NAME,
                                "logStatic",
                                "(Ljava/lang/String;)V",
                                false);

                        super.visitInsn(Opcodes.SWAP);
                    } else if (desc.equals("(Ljava/lang/String;)Ljava/lang/String;")) {
                        super.visitInsn(Opcodes.DUP);
                        super.visitLdcInsn("System.property.");   // Change to make the field name have System.property in front of it.
                        super.visitInsn(Opcodes.SWAP);

                        super.visitMethodInsn(Opcodes.INVOKESTATIC,
                                STATIC_TRACER_CLASS_NAME,
                                "concat",
                                "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;",
                                false);

                        super.visitMethodInsn(Opcodes.INVOKESTATIC,
                                STATIC_TRACER_CLASS_NAME,
                                "logStatic",
                                "(Ljava/lang/String;)V",
                                false);
                    }
                }
            }
        }

        super.visitMethodInsn(opcode, owner, name, desc, itf);
    }
}
