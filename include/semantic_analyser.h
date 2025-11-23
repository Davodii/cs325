#ifndef MC_SEMANTIC_ANALYSER_H
#define MC_SEMANTIC_ANALYSER_H

#include "ast.h"
#include "ast_visitor.h"
#include "error_reporter.h"
#include "symbol_table.h"
#include <memory>

class SemanticAnalyser : public ASTVisitor {
  public:
    SemanticAnalyser(ErrorReporter &errorReporter)
        : mErrorReporter(errorReporter), mSymbolTable() {
        // Add a initial global scope
        mSymbolTable.enterScope();
    }

    /**
     * @brief Run the semantic analysis on the given AST.
     *
     * @param program A unique pointer to the ProgramAST node representing the
     * program.
     */
    std::unique_ptr<ProgramAST> run(std::unique_ptr<ProgramAST> program);

  private:
    ErrorReporter &mErrorReporter;
    SymbolTable mSymbolTable;

    // Functions
    TYPE mCurrentReturnType = TYPE::VOID;
    bool mInsideFunction = false;

    void visit(ProgramAST &) override;
    void visit(IntToFloatCastAST &) override;
    void visit(IntASTnode &) override;
    void visit(BoolASTnode &) override;
    void visit(FloatASTnode &) override;
    void visit(VariableASTnode &) override;
    void visit(AssignExprAST &) override;
    void visit(BinaryExprAST &) override;
    void visit(UnaryExprAST &) override;
    void visit(ArgsAST &) override;
    void visit(CallExprAST &) override;
    void visit(ParamAST &) override;
    void visit(VarDeclAST &) override;
    void visit(GlobVarDeclAST &) override;
    void visit(BlockAST &) override;
    void visit(FunctionPrototypeAST &) override;
    void visit(FunctionDeclAST &) override;
    void visit(IfExprAST &) override;
    void visit(WhileExprAST &) override;
    void visit(ReturnAST &) override;
};

#endif