package edu.illinois.cs.dt.tools.fixer;

import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import org.apache.commons.io.FilenameUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

public class JavaMethod {
    public static Optional<JavaMethod> find(final String methodName, final List<Path> files,
                                            final String classpath)
            throws IOException {
        for (final Path file : files) {
            if (Files.exists(file) && FilenameUtils.isExtension(file.getFileName().toString(), "java")) {
                final JavaFile javaFile = JavaFile.loadFile(file, classpath, CleanerPathManager.compiledPath(file).getParent());

                final MethodDeclaration methodDeclaration = javaFile.findMethodDeclaration(methodName);

                if (methodDeclaration != null) {
                    return Optional.of(new JavaMethod(methodName, javaFile, methodDeclaration));
                }
            }
        }

        return Optional.empty();
    }

    private final String methodName;
    private final JavaFile javaFile;
    private final MethodDeclaration method;
    private final BlockStmt body;
    private final int beginLine;

    private JavaMethod(final String methodName, final JavaFile javaFile, final MethodDeclaration method) {
        this.methodName = methodName;
        this.javaFile = javaFile;
        this.method = method;
        this.body = method().getBody()
                .orElseThrow(() -> new IllegalArgumentException("Method " + methodName + " has no body!"));
        this.beginLine = body.getRange().get().begin.line;
    }

    public JavaFile javaFile() {
        return javaFile;
    }

    public MethodDeclaration method() {
        return method;
    }

    public String methodName() {
        return methodName;
    }

    public BlockStmt body() {
        return body;
    }

    public int beginLine() {
        return beginLine;
    }

    // Small helper to get class name from fully-qualified name of method
    public String getClassName() {
        return methodName.substring(0, methodName.lastIndexOf('.'));
    }

    public void prepend(final NodeList<Statement> stmts) {
        final NodeList<Statement> statements = body.getStatements();
        //statements.add(0, new BlockStmt(stmts));
        ClassOrInterfaceType exceptionType = new ClassOrInterfaceType().setName(new SimpleName("Exception"));
        CatchClause catchClause = new CatchClause(new Parameter(exceptionType, "ex"), new BlockStmt());
        statements.add(0, new TryStmt(new BlockStmt(stmts), NodeList.nodeList(catchClause), new BlockStmt()));

        javaFile().findMethodDeclaration(methodName()).setBody(new BlockStmt(statements));
    }

    // Helper method needed for delta-debugging to "reset" the state by removing that added first block
    public void removeFirstBlock() {
        final NodeList<Statement> statements = body.getStatements();
        statements.remove(0);

        javaFile().findMethodDeclaration(methodName()).setBody(new BlockStmt(statements));
    }

    public void append(final NodeList<Statement> stmts) {
        final NodeList<Statement> statements = body.getStatements();
        //statements.add(new BlockStmt(stmts));
        ClassOrInterfaceType exceptionType = new ClassOrInterfaceType().setName(new SimpleName("Exception"));
        CatchClause catchClause = new CatchClause(new Parameter(exceptionType, "ex"), new BlockStmt());
        statements.add(new TryStmt(new BlockStmt(stmts), NodeList.nodeList(catchClause), new BlockStmt()));

        javaFile().findMethodDeclaration(methodName()).setBody(new BlockStmt(statements));
    }

    // Helper method needed for delta-debugging to "reset" the state by removing that added first block
    public void removeLastBlock() {
        final NodeList<Statement> statements = body.getStatements();
        statements.remove(statements.size() - 1);

        javaFile().findMethodDeclaration(methodName()).setBody(new BlockStmt(statements));
    }
}
