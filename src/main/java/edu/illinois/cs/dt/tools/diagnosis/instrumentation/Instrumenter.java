package edu.illinois.cs.dt.tools.diagnosis.instrumentation;

import soot.Body;
import soot.BodyTransformer;
import soot.Local;
import soot.PatchingChain;
import soot.Scene;
import soot.SootClass;
import soot.SootField;
import soot.SootMethod;
import soot.Unit;
import soot.Value;
import soot.jimple.AssignStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IdentityStmt;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.Jimple;
import soot.jimple.ReturnStmt;
import soot.jimple.ReturnVoidStmt;
import soot.jimple.StaticInvokeExpr;
import soot.jimple.Stmt;
import soot.jimple.StringConstant;
import soot.options.Options;
import soot.tagkit.AnnotationTag;
import soot.tagkit.VisibilityAnnotationTag;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;

public class Instrumenter extends BodyTransformer {
    private final Path outputPath;

    private SootMethod logStatic;
    private SootMethod output;
    private SootMethod concat;

    public Instrumenter(final Path outputPath) {
        this.outputPath = outputPath;

        Scene.v().setSootClassPath(System.getProperty("java.class.path"));
        Options.v().set_whole_program(true);

        final SootClass staticTracer = Scene.v().loadClassAndSupport(StaticTracer.class.getCanonicalName());
        logStatic = staticTracer.getMethodByName("logStatic");
        output = staticTracer.getMethodByName("output");
        concat = staticTracer.getMethodByName("concat");
    }

    @Override
    protected void internalTransform(Body b, String phaseName, Map<String, String> options) {
        System.out.println("Instrumenting " + fqName(b.getMethod()));

        if (isJUnit4(b.getMethod())) {
            instrumentTestMethod(b);
        } else {
            instrumentMethod(b);
        }
    }

    private boolean isJUnit4(final SootMethod method) {
        final VisibilityAnnotationTag vat = (VisibilityAnnotationTag) method.getTag("VisibilityAnnotationTag");
        if (vat != null) {
            List<AnnotationTag> tags = vat.getAnnotations();
            for (AnnotationTag at : tags) {
                // TODO: Fix this so that it actually looks at the fq name for the Test annotation.
                if (at.getType().contains("Test")) {
                    return true;
                }
            }
        }

        return false;
    }

    private InvokeStmt staticInvoke(final SootMethod method, final Value... values) {
        return Jimple.v().newInvokeStmt(Jimple.v().newStaticInvokeExpr(method.makeRef(), values));
    }

    private void instrumentTestMethod(final Body b) {
        final PatchingChain<Unit> units = b.getUnits();

        // Don't instrument empty test methods
        if (units.getFirst() == units.getLast()) {
            return;
        }

        final String fqTestName = fqName(b.getMethod());

        final InvokeStmt invokeStmt = staticInvoke(output, StringConstant.v(outputPath.resolve(fqTestName).toString()));

        units.snapshotIterator().forEachRemaining(unit -> {
            instrumentFields(b, unit, units);

            if (unit instanceof ReturnVoidStmt || unit instanceof ReturnStmt) {
                units.insertBefore(staticInvoke(output, StringConstant.v(outputPath.resolve(fqTestName).toString())), unit);
            }
        });

        addCatch(b, units, invokeStmt);
    }

    /**
     * Insert a catch so that the test cannot exit without calling the output method.
     */
    private void addCatch(final Body b, final PatchingChain<Unit> units, final InvokeStmt invokeStmt) {
        final SootClass thrwCls = Scene.v().getSootClass("java.lang.Throwable");

        final GotoStmt gotoStmt = Jimple.v().newGotoStmt(units.getLast());
        units.insertBeforeNoRedirect(gotoStmt, units.getLast());

        final Local local = Jimple.v().newLocal("tmpCaughtException", thrwCls.getType());
        b.getLocals().add(local);
        final IdentityStmt catchStmt = Jimple.v().newIdentityStmt(local, Jimple.v().newCaughtExceptionRef());
        units.insertBeforeNoRedirect(catchStmt, units.getLast());

        b.getTraps().add(Jimple.v().newTrap(thrwCls, units.getFirst(), gotoStmt, catchStmt));

        units.insertBeforeNoRedirect(invokeStmt, units.getLast());
        units.insertBeforeNoRedirect(Jimple.v().newThrowStmt(local), units.getLast());

        // This is a test function, so the return type should be void.
//        units.insertAfter(Jimple.v().newReturnVoidStmt(), units.getLast());
    }

    private String fqName(final SootMethod method) {
        return method.getDeclaringClass().getName() + "." + method.getName();
    }

    private String fqName(final SootField field) {
        return field.getDeclaringClass().getName() + "." + field.getName();
    }

    private void instrumentFields(Body b, Unit unit, PatchingChain<Unit> units) {
        if (unit instanceof Stmt) {
            final Stmt statement = (Stmt) unit;

            if (statement.containsFieldRef() && statement.getFieldRef().getField().isStatic()) {
                final String fqName = fqName(statement.getFieldRef().getField());

                units.insertBefore(staticInvoke(logStatic, StringConstant.v(fqName)), unit);
            }

            if (statement.containsInvokeExpr()) {
                final InvokeExpr expr = statement.getInvokeExpr();
                final String fqName = fqName(expr.getMethod());

                if (fqName.equals("java.lang.System.getProperty")) {
                    final StaticInvokeExpr invoke = Jimple.v().newStaticInvokeExpr(concat.makeRef(), StringConstant.v("System.property."), expr.getArg(0));

                    final SootClass string = Scene.v().loadClassAndSupport(String.class.getCanonicalName());
                    final Local local = Jimple.v().newLocal("tmpSystemPropertyConcatName", string.getType());
                    b.getLocals().add(local);

                    final AssignStmt assignStmt = Jimple.v().newAssignStmt(local, invoke);
                    final InvokeStmt invokeStmt = staticInvoke(logStatic, local);

                    units.insertBefore(invokeStmt, unit);
                    units.insertBefore(assignStmt, invokeStmt);
                }
            }
        }
    }

    private void instrumentMethod(final Body b) {
        final PatchingChain<Unit> units = b.getUnits();
        units.snapshotIterator().forEachRemaining(unit -> instrumentFields(b, unit, units));
    }
}
