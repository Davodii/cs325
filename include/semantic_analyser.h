#ifndef MC_SEMANTIC_ANALYSER_H
#define MC_SEMANTIC_ANALYSER_H

#include "ast.h"
#include "symbol_table.h"
#include "error_reporter.h"
#include <memory>

class SemanticAnalyser {
  public:
    SemanticAnalyser(ErrorReporter &errorReporter)
        : mErrorReporter(errorReporter), mSymbolTable() {
        // Add a initial global scope
        mSymbolTable.enterScope();
    }

    /**
     * @brief Run the semantic analysis on the given AST.
     *
     * @param ast A vector of unique pointers to AST nodes representing the
     * program.
     */
    void run(std::vector<std::unique_ptr<ASTnode>> ast);

  private:
    ErrorReporter &mErrorReporter;

    SymbolTable mSymbolTable;
    // --- Recursive analysis methods ---
    // Take ownership of a node and return ownership of the
    // (potentially) new node.

    // For top-level declarations and statements;
    // void analyse(std::unique_ptr<ASTnode> &node);

    // For expressions. Returns a unique_ptr because
    // an expression can be replaced.
    std::unique_ptr<ExprAST>
    analyseExpression(std::unique_ptr<ExprAST> expression);

    // Specific helpers for different statement types

    /// TODO: add more helper functions as needed
};

#endif