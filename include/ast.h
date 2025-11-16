#ifndef MC_AST_H
#define MC_AST_H

#include <memory>
#include <vector>
#include <string>

#include "lexer.h"
// #include "ast_visitor.h"

enum TYPE {
  INT,
  FLOAT,
  BOOL,
};

// TODO: Why do we have this empty enum?
enum IDENT_TYPE { 
  IDENTIFIER = 0,
  FUNCTION = 1,
};

/**
 * @brief Base class for all AST nodes.
 * 
 */
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual std::string to_string() const { return ""; };
};

/**
 * @brief Base class for declaration AST nodes.
 * 
 */
class DeclAST : public ASTnode {
public:
  virtual ~DeclAST() {}
  virtual std::string to_string() const { return ""; };
};

// ----- Expressions -----
/**
 * @brief Abstract class for other expressions.
 * 
 */
class ExprAST : public ASTnode {
public:
  virtual ~ExprAST() = default;
  virtual TYPE getType();
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
  const TYPE getType() const { return TYPE::INT; }
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
  const TYPE getType() const { return TYPE::BOOL; }
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
  const TYPE getType() const { return TYPE::FLOAT; }
private:
  double mVal;
  TOKEN mToken;
};

/**
 * @brief Class for referencing a variable (i.e. identifier).
 * 
 */
class VariableASTnode : public ExprAST {
public:
  // TODO: Should not just be assiging mVarType to IDENTIFIER always.
  VariableASTnode(const TOKEN &tok, const std::string &name)
      : mToken(tok), mName(name), mVarType(IDENT_TYPE::IDENTIFIER)  {}
  const std::string &getName() const { return mName; }
  TYPE getType();
  const IDENT_TYPE getVarType() const { return mVarType; }
private:
  TOKEN mToken;
  std::string mName;
  IDENT_TYPE mVarType;
};

/**
 * @brief Class for assignment expressions.
 * 
 */
class AssignAST : public ExprAST {
public:
  AssignAST(std::unique_ptr<VariableASTnode> variable, std::unique_ptr<ExprAST> expression)
    : mVariable(std::move(variable)), mExpression(std::move(expression)) {}

  /// Return the type of the expression
  const TYPE getType() const { return mExpression.get()->getType();};
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
  BinaryExprAST(std::unique_ptr<ExprAST> left, TOKEN_TYPE op, std::unique_ptr<ExprAST> right)
    : mLeft(std::move(left)), mOp(op), mRight(std::move(right)) {}
  TYPE getType();
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
  
  /**
   * @brief Get the type of the unary expression.
   * 
   * Unary expression type is the same as the type of its expression since
   * unary operators should not change the type.
   * 
   * @return const TYPE 
   */
  const TYPE getType() const { return mExpression.get()->getType();};
private:
  TOKEN_TYPE mOp;
  std::unique_ptr<ExprAST> mExpression;
};

/**
 * @brief Class for a function call expression.
 * 
 */
class CallExprAST : public ExprAST {
public:
  CallExprAST(std::unique_ptr<VariableASTnode> callee, std::unique_ptr<ArgsAST> args) : 
    mCallee(std::move(callee)), mArgs(std::move(args)) {};
  const TYPE getType() const { return mCallee->getType();};
private:
  std::unique_ptr<VariableASTnode> mCallee;
  std::unique_ptr<ArgsAST> mArgs;
};

/**
 * @brief Class for function arguments in a function call.
 * 
 */
class ArgsAST : public ExprAST {
public:
  ArgsAST(std::vector<std::unique_ptr<ExprAST>> args)
      : ArgsList(std::move(args)) {}
private:
  std::vector<std::unique_ptr<ExprAST>> ArgsList;
};

/**
 * @brief Class for a function parameter.
 * 
 */
class ParamAST {
  std::string Name;
  TYPE Type;

public:
  ParamAST(const std::string &name, TYPE type)
      : Name(name), Type(type) {}
  const std::string &getName() const { return Name; }
  TYPE getType() { return Type; }
};

/**
 * @brief Class for a variable declaration.
 * 
 */
class VarDeclAST : public DeclAST {
  std::string Name;
  TYPE Type;

public:
  VarDeclAST(const std::string &name, TYPE type)
      : Name(name), Type(type) {}
  TYPE getType() { return Type; }
  const std::string &getName() const { return Name; }
};

/**
 * @brief Class for a global variable declaration.
 * 
 */
class GlobVarDeclAST : public DeclAST {
  std::string Name;
  TYPE Type;

public:
  GlobVarDeclAST(const std::string &name, TYPE type)
      : Name(name), Type(type) {}
  TYPE getType() { return Type; }
  const std::string &getName() const { return Name; }
};

/**
 * @brief Class for a function declaration's signature.
 * 
 */
class FunctionPrototypeAST {
  std::string Name;
  TYPE Type;
  std::vector<std::unique_ptr<ParamAST>> Params; // vector of parameters

public:
  FunctionPrototypeAST(const std::string &name, TYPE type,
                       std::vector<std::unique_ptr<ParamAST>> params)
      : Name(name), Type(type), Params(std::move(params)) {}

  const std::string &getName() const { return Name; }
  TYPE getType() { return Type; }
  int getSize() const { return Params.size(); }
  std::vector<std::unique_ptr<ParamAST>> &getParams() { return Params; }
};

/**
 * @brief Class for a block of statements.
 * 
 */
class BlockAST : public ASTnode {
  std::vector<std::unique_ptr<VarDeclAST>> LocalDecls; // vector of local decls
  std::vector<std::unique_ptr<ASTnode>> Stmts;         // vector of statements

public:
  BlockAST(std::vector<std::unique_ptr<VarDeclAST>> localDecls,
           std::vector<std::unique_ptr<ASTnode>> stmts)
      : LocalDecls(std::move(localDecls)), Stmts(std::move(stmts)) {}
};

/**
 * @brief Class for a function definition itself.
 * 
 */
class FunctionDeclAST : public DeclAST {
  std::unique_ptr<FunctionPrototypeAST> Proto;
  std::unique_ptr<BlockAST> Block;

public:
  FunctionDeclAST(std::unique_ptr<FunctionPrototypeAST> Proto,
                  std::unique_ptr<BlockAST> Block)
      : Proto(std::move(Proto)), Block(std::move(Block)) {}
};

/**
 * @brief Class for an if statement.
 * 
 */
class IfExprAST : public ASTnode {
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<ASTnode> Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> condition, std::unique_ptr<ASTnode> then,
            std::unique_ptr<ASTnode> _else)
      : Condition(std::move(condition)), Then(std::move(then)), Else(std::move(_else)) {}
};

/**
 * @brief Class for a while statement.
 * 
 */
class WhileExprAST : public ASTnode {
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<ASTnode> Body;

public:
  WhileExprAST(std::unique_ptr<ExprAST> condition, std::unique_ptr<ASTnode> body)
      : Condition(std::move(condition)), Body(std::move(body)) {}
};

/**
 * @brief Class for a return statement.
 * 
 */
class ReturnAST : public ASTnode {
  std::unique_ptr<ExprAST> mExpression;

public:
  ReturnAST(std::unique_ptr<ExprAST> expression) : mExpression(std::move(expression)) {}
};
#endif