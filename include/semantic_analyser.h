#ifndef MC_SEMANTIC_ANALYSER_H
#define MC_SEMANTIC_ANALYSER_H

#include "ast_visitor.h"
#include "symbol_table.h"

class SemanticAnalyser {
public:
    SemanticAnalyser();

    /**
     * @brief Run the semantic analysis on the given AST.
     * 
     * @param ast A vector of unique pointers to AST nodes representing the program.
     */
    void run(std::vector<std::unique_ptr<ASTnode>> ast);
private:
    SymbolTable mSymbolTable;

    /// TODO: expand this to actually include good error messages
    bool hasError = false;

    // --- Recursive analysis methods ---
    // Take ownership of a node and return ownership of the
    // (potentially) new node.

    // For top-level declarations and statements;
    void analyse(std::unique_ptr<ASTnode> &node);

    // For expressions. Returns a unique_ptr because
    // an expression can be replaced.
    std::unique_ptr<ExprAST> analyseExpression(std::unique_ptr<ExprAST> expression);

    // Specific helpers for different statement types
    void analyseBlock(BlockAST *block);
    void analyseIf(IfExprAST *ifStmt);
    void analyseWhile(WhileExprAST *whileStmt);
    void analyseReturn(ReturnAST *returnStmt);
    void analyseVarDeclaration(VarDeclAST *variableDeclaration);
    void analyseFunctionDeclaration(FunctionDeclAST *functionDeclaration);

    /// TODO: add more helper functions as needed
};

#endif