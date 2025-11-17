#ifndef MC_AST_H
#define MC_AST_H

#include "lexer.h"
#include "symbol.h"
#include "types.h"
#include <cassert>
#include <memory>
#include <string>
#include <vector>

/**
 * @brief Base class for all AST nodes.
 *
 */
class ASTnode {
  public:
    virtual ~ASTnode() {}
    virtual std::string to_string(int indent = 0) const;
protected:
    std::string indentString(int indent) const {
        return std::string(indent * 2, ' '); // 2 spaces per level
    }
    std::string typeToString(TYPE type) const;
};

/**
 * @brief Base class for declaration AST nodes.
 *
 */
class DeclAST : public ASTnode {
  public:
    virtual ~DeclAST() {}
    virtual std::string to_string(int indent = 0) const;
};

// ----- Expressions -----
/**
 * @brief Abstract class for other expressions.
 *
 */
class ExprAST : public ASTnode {
  public:
    virtual ~ExprAST() = default;
    virtual std::string to_string(int indent = 0) const;
protected:
    std::string operatorToString(TOKEN_TYPE op) const;
  private:
    TYPE mType;
};

/**
 * @brief Node class for integer literals.
 *
 */
class IntASTnode : public ExprAST {
  public:
    IntASTnode(const TOKEN &tok, int val) : mVal(val), mToken(tok) {}
    std::string to_string(int indent = 0) const;

  private:
    int mVal;
    TOKEN mToken;
};

/**
 * @brief Node class for boolean literals true and false.
 *
 */
class BoolASTnode : public ExprAST {
  public:
    BoolASTnode(const TOKEN &tok, bool B) : mBool(B), mToken(tok) {}
    std::string to_string(int indent = 0) const;

  private:
    bool mBool;
    TOKEN mToken;
};

/**
 * @brief Node class for floating point literals.
 *
 */
class FloatASTnode : public ExprAST {
  public:
    FloatASTnode(const TOKEN &tok, double val) : mVal(val), mToken(tok) {}
    std::string to_string(int indent = 0) const;

  private:
    double mVal;
    TOKEN mToken;
};

/**
 * @brief Class for referencing a variable (i.e. identifier).
 *
 * Only used for variable references in expressions.
 *
 */
class VariableASTnode : public ExprAST {
  public:
    VariableASTnode(const TOKEN &tok, const std::string &name)
        : mToken(tok), mName(name) {}
    const std::string &getName() const { return mName; }
    std::string to_string(int indent = 0) const;

    Symbol *resolvedSymbol;

  private:
    TOKEN mToken;
    std::string mName;
};

/**
 * @brief Class for assignment expressions.
 *
 */
class AssignExprAST : public ExprAST {
  public:
    AssignExprAST(std::unique_ptr<VariableASTnode> variable,
                  std::unique_ptr<ExprAST> expression)
        : mVariable(std::move(variable)), mExpression(std::move(expression)) {}

    /// Return the type of the expression
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<VariableASTnode> mVariable;
    std::unique_ptr<ExprAST> mExpression;
};

/**
 * @brief Class for binary expressions.
 *
 */
class BinaryExprAST : public ExprAST {
  public:
    BinaryExprAST(std::unique_ptr<ExprAST> left, TOKEN_TYPE op,
                  std::unique_ptr<ExprAST> right)
        : mLeft(std::move(left)), mOp(op), mRight(std::move(right)) {}
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<ExprAST> mLeft, mRight;
    TOKEN_TYPE mOp;
};

/**
 * @brief Class for unary expressions.
 *
 */
class UnaryExprAST : public ExprAST {
  public:
    UnaryExprAST(TOKEN_TYPE op, std::unique_ptr<ExprAST> expression)
        : mOp(op), mExpression(std::move(expression)) {}

    std::string to_string(int indent = 0) const;

  private:
    TOKEN_TYPE mOp;
    std::unique_ptr<ExprAST> mExpression;
};

/**
 * @brief Class for function arguments in a function call.
 *
 */
class ArgsAST : public ExprAST {
  public:
    ArgsAST(std::vector<std::unique_ptr<ExprAST>> args)
        : ArgsList(std::move(args)) {}

    std::string to_string(int indent = 0) const;

  private:
    std::vector<std::unique_ptr<ExprAST>> ArgsList;
};

/**
 * @brief Class for a function call expression.
 *
 */
class CallExprAST : public ExprAST {
  public:
    CallExprAST(std::unique_ptr<VariableASTnode> callee,
                std::unique_ptr<ArgsAST> args)
        : mCallee(std::move(callee)), mArgs(std::move(args)) {};
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<VariableASTnode> mCallee;
    std::unique_ptr<ArgsAST> mArgs;
};

/**
 * @brief Class for a function parameter.
 *
 */
class ParamAST : public ASTnode {
  public:
    ParamAST(const std::string &name, TYPE type) : mName(name), mType(type) {}
    const std::string &getName() const { return mName; }
    std::string to_string(int indent = 0) const;

    // TODO: make symbol be private and use getter/setter
    Symbol *symbol;

  private:
    std::string mName;
    TYPE mType;
};

/**
 * @brief Class for a variable declaration.
 *
 */
class VarDeclAST : public DeclAST {
  public:
    VarDeclAST(const std::string &name, TYPE type) : mName(name), mType(type) {}
    const std::string &getName() const { return mName; }
    std::string to_string(int indent = 0) const;

    Symbol *symbol;

  private:
    std::string mName;
    TYPE mType;
};

/**
 * @brief Class for a global variable declaration.
 *
 */
class GlobVarDeclAST : public DeclAST {
  public:
    GlobVarDeclAST(const std::string &name, TYPE type)
        : mName(name), mType(type) {}
    const std::string &getName() const { return mName; }
    std::string to_string(int indent = 0) const;

    Symbol *symbol;

  private:
    std::string mName;
    TYPE mType;
};

/**
 * @brief Class for a block of statements.
 *
 */
class BlockAST : public ASTnode {
  public:
    BlockAST(std::vector<std::unique_ptr<VarDeclAST>> localDecls,
             std::vector<std::unique_ptr<ASTnode>> stmts)
        : mLocalDecls(std::move(localDecls)), mStmts(std::move(stmts)) {}
    std::string to_string(int indent = 0) const;

  private:
    std::vector<std::unique_ptr<VarDeclAST>>
        mLocalDecls;                              // vector of local decls
    std::vector<std::unique_ptr<ASTnode>> mStmts; // vector of statements
};

/**
 * @brief Class for a function declaration's signature.
 *
 */
class FunctionPrototypeAST : public ASTnode {
  public:
    FunctionPrototypeAST(const std::string &name, TYPE type,
                         std::vector<std::unique_ptr<ParamAST>> params)
        : mName(name), mType(type), mParams(std::move(params)) {}

    const std::string &getName() const { return mName; }
    int getSize() const { return mParams.size(); }
    std::vector<std::unique_ptr<ParamAST>> &getParams() { return mParams; }
    std::string to_string(int indent = 0) const;

    Symbol *symbol;

  private:
    std::string mName;
    TYPE mType;
    std::vector<std::unique_ptr<ParamAST>> mParams;
};

/**
 * @brief Class for a function definition itself.
 *
 */
class FunctionDeclAST : public DeclAST {
  public:
    FunctionDeclAST(std::unique_ptr<FunctionPrototypeAST> mProto,
                    std::unique_ptr<BlockAST> mBlock)
        : mProto(std::move(mProto)), mBlock(std::move(mBlock)) {}
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<FunctionPrototypeAST> mProto;
    std::unique_ptr<BlockAST> mBlock;
};

/**
 * @brief Class for an if statement.
 *
 */
class IfExprAST : public ASTnode {
  public:
    IfExprAST(std::unique_ptr<ExprAST> condition,
              std::unique_ptr<BlockAST> then, std::unique_ptr<BlockAST> _else)
        : mCondition(std::move(condition)), mThen(std::move(then)),
          mElse(std::move(_else)) {}
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<ExprAST> mCondition;
    std::unique_ptr<BlockAST> mThen, mElse;
};

/**
 * @brief Class for a while statement.
 *
 */
class WhileExprAST : public ASTnode {
  public:
    WhileExprAST(std::unique_ptr<ExprAST> condition,
                 std::unique_ptr<BlockAST> body)
        : mCondition(std::move(condition)), mBody(std::move(body)) {}
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<ExprAST> mCondition;
    std::unique_ptr<BlockAST> mBody;
};

/**
 * @brief Class for a return statement.
 *
 */
class ReturnAST : public ASTnode {
  public:
    ReturnAST(std::unique_ptr<ExprAST> expression)
        : mExpression(std::move(expression)) {}
    std::string to_string(int indent = 0) const;

  private:
    std::unique_ptr<ExprAST> mExpression;
};
#endif