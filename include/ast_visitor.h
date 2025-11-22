#ifndef MC_AST_VISITOR_H
#define MC_AST_VISITOR_H

// Forward declare all the AST nodes we are using
class ProgramAST;
class IntASTnode;
class BoolASTnode;
class FloatASTnode;
class VariableASTnode;
class AssignExprAST;
class BinaryExprAST;
class UnaryExprAST;
class ArgsAST;
class CallExprAST;
class ParamAST;
class VarDeclAST;
class GlobVarDeclAST;
class BlockAST;
class FunctionPrototypeAST;
class FunctionDeclAST;
class IfExprAST;
class WhileExprAST;
class ReturnAST;

class ASTVisitor {
  public:
    virtual ~ASTVisitor() = default;

    virtual void visit(ProgramAST&) = 0;
    virtual void visit(IntASTnode&) = 0;
    virtual void visit(BoolASTnode&) = 0;
    virtual void visit(FloatASTnode&) = 0;
    virtual void visit(VariableASTnode&) = 0;
    virtual void visit(AssignExprAST&) = 0;
    virtual void visit(BinaryExprAST&) = 0;
    virtual void visit(UnaryExprAST&) = 0;
    virtual void visit(ArgsAST&) = 0;
    virtual void visit(CallExprAST&) = 0;
    virtual void visit(ParamAST&) = 0;
    virtual void visit(VarDeclAST&) = 0;
    virtual void visit(GlobVarDeclAST&) = 0;
    virtual void visit(BlockAST&) = 0;
    virtual void visit(FunctionPrototypeAST&) = 0;
    virtual void visit(FunctionDeclAST&) = 0;
    virtual void visit(IfExprAST&) = 0;
    virtual void visit(WhileExprAST&) = 0;
    virtual void visit(ReturnAST&) = 0;
};

#endif