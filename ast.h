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

enum BINARY_OP {
  // Arithmetic
  ADD,    // +
  SUB,    // -
  MUL,    // *
  DIV,    // /
  MOD,    // %

  // Relational
  LT,     // <
  GT,     // >
  LE,     // <=
  GE,     // >=

  // Logical
  OR,     // ||
  AND,    // &&

  // Equality
  EQ,     // ==
  NEQ,    // !=
};

enum UNARY_OP {
  NEG,    // -
  NOT,    // !
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
  TYPE mType;
public:
  virtual ~ExprAST() = default;
  virtual TYPE getType();
};

/**
 * @brief Node class for integer literals.
 * 
 */
class IntASTnode : public ExprAST {
  int mVal;
  TOKEN mToken;

public:
  IntASTnode(TOKEN tok, int val) : mVal(val), mToken(tok) {}
  const TYPE getType() const { return TYPE::INT; }
};

/**
 * @brief Node class for boolean literals true and false.
 * 
 */
class BoolASTnode : public ExprAST {
  bool mBool;
  TOKEN mToken;

public:
  BoolASTnode(TOKEN tok, bool B) : mBool(B), mToken(tok) {}
  const TYPE getType() const { return TYPE::BOOL; }
};

/**
 * @brief Node class for floating point literals.
 * 
 */
class FloatASTnode : public ExprAST {
  double mVal;
  TOKEN mToken;

public:
  FloatASTnode(TOKEN tok, double mVal) : mVal(mVal), mToken(tok) {}
  const TYPE getType() const { return TYPE::FLOAT; }
};

/**
 * @brief Class for referencing a variable (i.e. identifier).
 * 
 */
class VariableASTnode : public ExprAST {
protected:
  TOKEN mToken;
  std::string Name;

  // TODO: Why is this here?
  IDENT_TYPE VarType;

public:
  VariableASTnode(TOKEN tok, const std::string &Name)
      : mToken(tok), Name(Name), VarType(IDENT_TYPE::IDENTIFIER)  {}
  const std::string &getName() const { return Name; }
  TYPE getType();
  const IDENT_TYPE getVarType() const { return VarType; }
};

/**
 * @brief Class for assignment expressions.
 * 
 */
class AssignAST : public ExprAST {
  std::unique_ptr<VariableASTnode> Variable;
  std::unique_ptr<ExprAST> Expression;

public:
  AssignAST(std::unique_ptr<VariableASTnode> variable, std::unique_ptr<ExprAST> expression);

  /// Return the type of the expression
  const TYPE getType() const { return Expression.get()->getType();};
};

/**
 * @brief Class for binary expressions.
 * 
 */
class BinaryExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Left;
  BINARY_OP Op;
  std::unique_ptr<ExprAST> Right;

public:
  BinaryExprAST(std::unique_ptr<ExprAST> left, char op, std::unique_ptr<ExprAST> right);
  TYPE getType();
};

/**
 * @brief Class for unary expressions.
 * 
 */
class UnaryExprAST : public ExprAST {
  UNARY_OP Op;
  std::unique_ptr<ExprAST> Expression;

public:
  UnaryExprAST(UNARY_OP op, std::unique_ptr<ExprAST> expression);
  
  // Unary expression should not change (promote or demote values).
  const TYPE getType() const { return Expression.get()->getType();};
};

/**
 * @brief Class for a function call expression.
 * 
 */
class CallExprAST : public ExprAST {
  std::unique_ptr<VariableASTnode> Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(std::unique_ptr<VariableASTnode> callee, std::vector<std::unique_ptr<ExprAST>> args) : 
    Callee(std::move(callee)), Args(std::move(args)) {};
  const TYPE getType() const { return Callee->getType();};
};

/**
 * @brief Class for function arguments in a function call.
 * 
 */
class ArgsAST : public ExprAST {
  std::unique_ptr<VariableASTnode> Callee;
  std::vector<std::unique_ptr<ExprAST>> ArgsList;

public:
  ArgsAST(std::unique_ptr<VariableASTnode> callee, std::vector<std::unique_ptr<ExprAST>> args)
      : Callee(std::move(callee)), ArgsList(std::move(args)) {}
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
  std::unique_ptr<ExprAST> Expression;

public:
  ReturnAST(std::unique_ptr<ExprAST> expression) : Expression(std::move(expression)) {}
};
#endif