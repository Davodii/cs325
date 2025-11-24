#ifndef MC_SEMANTIC_ANALYSER_H
#define MC_SEMANTIC_ANALYSER_H

#include "ast.h"
#include "ast_visitor.h"
#include "error_reporter.h"
#include <map>
#include <string>
#include <memory>
#include <vector>

class Symbol {
  public:
    Symbol() = default;
    Symbol(const std::string &name, TYPE type, IDENT_TYPE identType,
           ASTnode *declaration = nullptr)
        : name(name), type(type), identType(identType),
          declaration(declaration) {}

    std::string getName() const { return name; }
    TYPE getType() const { return type; }
    IDENT_TYPE getKind() const { return identType; }
    ASTnode *getDeclaration() const { return declaration; }

  private:
    std::string name;
    TYPE type;
    IDENT_TYPE identType;
    ASTnode
        *declaration; // Pointer to the AST node where the symbol is declared
};

class SymbolTable {
    std::vector<std::map<std::string, Symbol>> scopeStack;

  public:
    SymbolTable();

    /**
     * @brief Enter a new scope.
     *
     */
    void enterScope();

    /**
     * @brief Leave the current scope
     *
     */
    void leaveScope();

    /**
     * @brief Add a symbol to the current scope.
     *
     * @param symbol The symbol to add.
     * @return true if the symbol was added successfully.
     * @return false if there was a re-declaration in the current scope.
     */
    bool addSymbol(Symbol symbol);

    /**
     * @brief Lookup a symbol by name, searching from the innermost scope to the
     * outermost.
     *
     * @param name The name of the symbol to look up.
     * @return Symbol* Pointer to the symbol if found, nullptr otherwise.
     */
    Symbol* lookup(const std::string &name);

    /**
     * @brief Check if a symbol is defined in the current scope.
     *
     * @param name The name of the symbol to check.
     * @return true If the symbol is defined in the current scope.
     * @return false If the symbol is not defined in the current scope.
     */
    bool isDefinedInCurrentScope(const std::string &name) const;
};

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
    bool mDefiningExtern = false;

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