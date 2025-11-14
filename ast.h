#ifndef MC_AST_H
#define MC_AST_H

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <memory>
#include <vector>
#include <string>

#include "lexer.h"

using namespace llvm;
using namespace llvm::sys;

// TODO: check if can forward declare llvm classes to cut down on includes
enum TYPE {
  INT,
  FLOAT,
  BOOL,
};

// TODO: Why do we have this empty enum?
enum IDENT_TYPE { 
  IDENTIFIER = 0 
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

/// ASTnode - Base class for all AST nodes.
/// Abstract class that provides nothing.
class ASTnode {

public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const { return ""; };
};

/// DeclAST - Base class for declarations, variables and functions
class DeclAST : public ASTnode {

public:
  virtual ~DeclAST() {}
};

// // NOTE: This is just a Binary operator Node
// class ExprAST : public ASTnode {
//   std::unique_ptr<ASTnode> Node1;
//   char Op;
//   std::unique_ptr<ASTnode> Node2;

// public:
//   ExprAST(std::unique_ptr<ASTnode> node1, char op,
//           std::unique_ptr<ASTnode> node2)
//       : Node1(std::move(node1)), Op(op), Node2(std::move(node2)) {}
//   Value *codegen();
//   virtual TYPE getType();
// };

// ----- Expressions -----
/// ExprART - Abstract class for other expressions.
class ExprAST : public ASTnode {
public:
  virtual ~ExprAST() = default;
  virtual Value *codegen() = 0;
  virtual TYPE getType();
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ExprAST {
  int Val;
  TOKEN Tok;
  // std::string Name;

public:
  IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  const TYPE getType() const { return TYPE::INT; }
};

/// BoolASTnode - Class for boolean literals true and false,
class BoolASTnode : public ExprAST {
  bool Bool;
  TOKEN Tok;

public:
  BoolASTnode(TOKEN tok, bool B) : Bool(B), Tok(tok) {}
  virtual Value *codegen();
  const TYPE getType() const { return TYPE::BOOL; }
};

/// FloatASTnode - Node class for floating point literals like "1.0".
class FloatASTnode : public ExprAST {
  double Val;
  TOKEN Tok;

public:
  FloatASTnode(TOKEN tok, double Val) : Val(Val), Tok(tok) {}
  virtual Value *codegen();
  const TYPE getType() const { return TYPE::FLOAT; }
};

/// VariableASTnode - Class for referencing a variable (i.e. identifier), like
/// "a".
class VariableASTnode : public ExprAST {
protected:
  TOKEN Tok;
  std::string Name;

  // TODO: Why is this here?
  IDENT_TYPE VarType;

public:
  VariableASTnode(TOKEN tok, const std::string &Name)
      : Tok(tok), Name(Name), VarType(IDENT_TYPE::IDENTIFIER)  {}
  const std::string &getName() const { return Name; }
  TYPE getType();
  const IDENT_TYPE getVarType() const { return VarType; }
  virtual Value *codegen();
};

class AssignAST : public ExprAST {
  std::unique_ptr<VariableASTnode> Variable;
  std::unique_ptr<ExprAST> Expression;

public:
  AssignAST(std::unique_ptr<VariableASTnode> variable, std::unique_ptr<ExprAST> expression);
  Value *codegen();

  /// Return the type of the expression
  const TYPE getType() const { return Expression.get()->getType();};
};

class BinaryExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Left;
  BINARY_OP Op;
  std::unique_ptr<ExprAST> Right;

public:
  BinaryExprAST(std::unique_ptr<ExprAST> left, char op, std::unique_ptr<ExprAST> right);
  Value *codegen();
  TYPE getType();
};

enum UNARY_OP {
  NEG,    // -
  NOT,    // !
};
class UnaryExprAST : public ExprAST {
  UNARY_OP Op;
  std::unique_ptr<ExprAST> Expression;

public:
  UnaryExprAST(UNARY_OP op, std::unique_ptr<ExprAST> expression);
  Value *codegen();
  
  // Unary expression should not change (promote or demote values).
  const TYPE getType() const { return Expression.get()->getType();};
};

class CallExprAST : public ExprAST {
  std::unique_ptr<VariableASTnode> Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(std::unique_ptr<VariableASTnode> callee, std::vector<std::unique_ptr<ExprAST>> args) : 
    Callee(std::move(callee)), Args(std::move(args)) {};
  Value *codegen();
  const TYPE getType() const { return Callee->getType();};
};

/// ArgsAST - Class for a function argument in a function call
class ArgsAST : public ExprAST {
  std::unique_ptr<VariableASTnode> Callee;
  std::vector<std::unique_ptr<ExprAST>> ArgsList;

public:
  ArgsAST(std::unique_ptr<VariableASTnode> callee, std::vector<std::unique_ptr<ExprAST>> args)
      : Callee(std::move(callee)), ArgsList(std::move(args)) {}

  Value *codegen() override;
};
// ----- End -----

/// ParamAST - Class for a parameter declaration
class ParamAST {
  std::string Name;
  TYPE Type;

public:
  ParamAST(const std::string &name, TYPE type)
      : Name(name), Type(type) {}
  const std::string &getName() const { return Name; }
  TYPE getType() { return Type; }
};

/// VarDeclAST - Class for a variable declaration
class VarDeclAST : public DeclAST {
  std::string Name;
  TYPE Type;

public:
  VarDeclAST(const std::string &name, TYPE type)
      : Name(name), Type(type) {}
  Value *codegen();
  TYPE getType() { return Type; }
  const std::string &getName() const { return Name; }
};

/// GlobVarDeclAST - Class for a Global variable declaration
class GlobVarDeclAST : public DeclAST {
  std::string Name;
  TYPE Type;

public:
  GlobVarDeclAST(const std::string &name, TYPE type)
      : Name(name), Type(type) {}
  Value *codegen();
  TYPE getType() { return Type; }
  const std::string &getName() const { return Name; }
};

/// FunctionPrototypeAST - Class for a function declaration's signature
class FunctionPrototypeAST {
  std::string Name;
  TYPE Type;
  std::vector<std::unique_ptr<ParamAST>> Params; // vector of parameters

public:
  FunctionPrototypeAST(const std::string &name, TYPE type,
                       std::vector<std::unique_ptr<ParamAST>> params)
      : Name(name), Type(type), Params(std::move(params)) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
  TYPE getType() { return Type; }
  int getSize() const { return Params.size(); }
  std::vector<std::unique_ptr<ParamAST>> &getParams() { return Params; }
};

/// BlockAST - Class for a block with declarations followed by statements
class BlockAST : public ASTnode {
  std::vector<std::unique_ptr<VarDeclAST>> LocalDecls; // vector of local decls
  std::vector<std::unique_ptr<ASTnode>> Stmts;         // vector of statements

public:
  BlockAST(std::vector<std::unique_ptr<VarDeclAST>> localDecls,
           std::vector<std::unique_ptr<ASTnode>> stmts)
      : LocalDecls(std::move(localDecls)), Stmts(std::move(stmts)) {}
  // BasicBlock *codegen();
  Value *codegen(Function *TheFunction);
  Value *codegen();
};

/// FunctionDeclAST - This class represents a function definition itself.
class FunctionDeclAST : public DeclAST {
  std::unique_ptr<FunctionPrototypeAST> Proto;
  std::unique_ptr<BlockAST> Block;

public:
  FunctionDeclAST(std::unique_ptr<FunctionPrototypeAST> Proto,
                  std::unique_ptr<BlockAST> Block)
      : Proto(std::move(Proto)), Block(std::move(Block)) {}
  Function *codegen();
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ASTnode {
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<ASTnode> Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> condition, std::unique_ptr<ASTnode> then,
            std::unique_ptr<ASTnode> _else)
      : Condition(std::move(condition)), Then(std::move(then)), Else(std::move(_else)) {}

  Value *codegen() override;
};

/// WhileExprAST - Expression class for while.
class WhileExprAST : public ASTnode {
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<ASTnode> Body;

public:
  WhileExprAST(std::unique_ptr<ExprAST> condition, std::unique_ptr<ASTnode> body)
      : Condition(std::move(condition)), Body(std::move(body)) {}

  Value *codegen() override;
};

/// ReturnAST - Class for a return value
class ReturnAST : public ASTnode {
  std::unique_ptr<ExprAST> Expression;

public:
  ReturnAST(std::unique_ptr<ExprAST> expression) : Expression(std::move(expression)) {}

  Value *codegen() override;
};
#endif