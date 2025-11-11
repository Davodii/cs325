#ifndef AST_H
#define AST_H

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

/// ASTnode - Base class for all AST nodes.
class ASTnode {

public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const { return ""; };
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
  int Val;
  TOKEN Tok;
  std::string Name;

public:
  IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  virtual Value *codegen() override;
  const std::string &getType() const { return Tok.lexeme; }
};

/// BoolASTnode - Class for boolean literals true and false,
class BoolASTnode : public ASTnode {
  bool Bool;
  TOKEN Tok;

public:
  BoolASTnode(TOKEN tok, bool B) : Bool(B), Tok(tok) {}
  virtual Value *codegen();
  const std::string &getType() const { return Tok.lexeme; }
};

/// FloatASTnode - Node class for floating point literals like "1.0".
class FloatASTnode : public ASTnode {
  double Val;
  TOKEN Tok;

public:
  FloatASTnode(TOKEN tok, double Val) : Val(Val), Tok(tok) {}
  virtual Value *codegen();
  const std::string &getType() const { return Tok.lexeme; }
};

/// VariableASTnode - Class for referencing a variable (i.e. identifier), like
/// "a".
enum IDENT_TYPE { IDENTIFIER = 0 };
class VariableASTnode : public ASTnode {
protected:
  TOKEN Tok;
  std::string Name;
  IDENT_TYPE VarType;

public:
  VariableASTnode(TOKEN tok, const std::string &Name)
      : Tok(tok), Name(Name), VarType(IDENT_TYPE::IDENTIFIER) {}
  const std::string &getName() const { return Name; }
  const std::string &getType() const { return Tok.lexeme; }
  const IDENT_TYPE getVarType() const { return VarType; }
  virtual Value *codegen();
};

/// ParamAST - Class for a parameter declaration
class ParamAST {
  std::string Name;
  std::string Type;

public:
  ParamAST(const std::string &name, const std::string &type)
      : Name(name), Type(type) {}
  const std::string &getName() const { return Name; }
  const std::string &getType() const { return Type; }
};

/// DeclAST - Base class for declarations, variables and functions
class DeclAST : public ASTnode {

public:
  virtual ~DeclAST() {}
};

/// VarDeclAST - Class for a variable declaration
class VarDeclAST : public DeclAST {
  std::unique_ptr<VariableASTnode> Var;
  std::string Type;

public:
  VarDeclAST(std::unique_ptr<VariableASTnode> var, const std::string &type)
      : Var(std::move(var)), Type(type) {}
  Value *codegen();
  const std::string &getType() const { return Type; }
  const std::string &getName() const { return Var->getName(); }
};

/// GlobVarDeclAST - Class for a Global variable declaration
class GlobVarDeclAST : public DeclAST {
  std::unique_ptr<VariableASTnode> Var;
  std::string Type;

public:
  GlobVarDeclAST(std::unique_ptr<VariableASTnode> var, const std::string &type)
      : Var(std::move(var)), Type(type) {}
  Value *codegen();
  const std::string &getType() const { return Type; }
  const std::string &getName() const { return Var->getName(); }
};

/// FunctionPrototypeAST - Class for a function declaration's signature
class FunctionPrototypeAST {
  std::string Name;
  std::string Type;
  std::vector<std::unique_ptr<ParamAST>> Params; // vector of parameters

public:
  FunctionPrototypeAST(const std::string &name, const std::string &type,
                       std::vector<std::unique_ptr<ParamAST>> params)
      : Name(name), Type(type), Params(std::move(params)) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
  const std::string &getType() const { return Type; }
  int getSize() const { return Params.size(); }
  std::vector<std::unique_ptr<ParamAST>> &getParams() { return Params; }
};

class ExprAST : public ASTnode {
  std::unique_ptr<ASTnode> Node1;
  char Op;
  std::unique_ptr<ASTnode> Node2;

public:
  ExprAST(std::unique_ptr<ASTnode> node1, char op,
          std::unique_ptr<ASTnode> node2)
      : Node1(std::move(node1)), Op(op), Node2(std::move(node2)) {}
  Value *codegen();
  const std::string &getType();
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
  std::unique_ptr<ASTnode> Block;

public:
  FunctionDeclAST(std::unique_ptr<FunctionPrototypeAST> Proto,
                  std::unique_ptr<ASTnode> Block)
      : Proto(std::move(Proto)), Block(std::move(Block)) {}
  Function *codegen();
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ASTnode {
  std::unique_ptr<ASTnode> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ASTnode> Cond, std::unique_ptr<ASTnode> Then,
            std::unique_ptr<ASTnode> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  Value *codegen() override;
};

/// WhileExprAST - Expression class for while.
class WhileExprAST : public ASTnode {
  std::unique_ptr<ASTnode> Cond, Body;

public:
  WhileExprAST(std::unique_ptr<ASTnode> cond, std::unique_ptr<ASTnode> body)
      : Cond(std::move(cond)), Body(std::move(body)) {}

  Value *codegen() override;
};

/// ReturnAST - Class for a return value
class ReturnAST : public ASTnode {
  std::unique_ptr<ASTnode> Val;

public:
  ReturnAST(std::unique_ptr<ASTnode> value) : Val(std::move(value)) {}

  Value *codegen() override;
};

/// ArgsAST - Class for a function argumetn in a function call
class ArgsAST : public ASTnode {
  std::string Callee;
  std::vector<std::unique_ptr<ASTnode>> ArgsList;

public:
  ArgsAST(const std::string &Callee, std::vector<std::unique_ptr<ASTnode>> list)
      : Callee(Callee), ArgsList(std::move(list)) {}

  Value *codegen() override;
};


#endif