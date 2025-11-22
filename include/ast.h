#ifndef MC_AST_H
#define MC_AST_H

#include "lexer.h"
#include "source_location.h"
#include "symbol.h"
#include "types.h"
#include <cassert>
#include <memory>
#include <string>
#include <vector>
#include "ast_visitor.h"

/**
 * @brief Base class for all AST nodes.
 *
 */
class ASTnode {
  public:
    ASTnode(SourceLoc sourceLoc) : mSourceLoc(sourceLoc) {}
    virtual ~ASTnode() {}
    virtual std::string to_string(int indent = 0) const;
    virtual void accept(ASTVisitor &v) = 0;
    SourceLoc getSourceLocation() { return mSourceLoc; }

  protected:
    std::string indentString(int indent) const {
        return std::string(indent * 2, ' '); // 2 spaces per level
    }

    SourceLoc mSourceLoc;
};

/**
 * @brief Base class for declaration AST nodes.
 *
 */
class DeclAST : public ASTnode {
  public:
    DeclAST(SourceLoc sourceLoc) : ASTnode(sourceLoc) {}
    virtual ~DeclAST() {}
    virtual std::string to_string(int indent = 0) const;
};

class ProgramAST : public ASTnode {
    public:
    ProgramAST(SourceLoc sourceLoc, std::vector<std::unique_ptr<ASTnode>> externList, std::vector<std::unique_ptr<ASTnode>> declaratioList) :
    ASTnode(sourceLoc), mExternList(externList), mDeclarationList(declaratioList) {}

    virtual ~ProgramAST() = default;
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    std::vector<std::unique_ptr<ASTnode>> getExternList() { return mExternList; }
    std::vector<std::unique_ptr<ASTnode>> getDeclarationList() {return mDeclarationList; }
private: 
    std::vector<std::unique_ptr<ASTnode>> mExternList;
    std::vector<std::unique_ptr<ASTnode>> mDeclarationList;
};

// ----- Expressions -----
/**
 * @brief Abstract class for other expressions.
 *
 */
class ExprAST : public ASTnode {
  public:
    ExprAST(SourceLoc sourceLoc) : ASTnode(sourceLoc) {}
    virtual ~ExprAST() = default;
    virtual std::string to_string(int indent = 0) const;

    TYPE getType() const { return mInferredType; };
    void setType(TYPE type) { mInferredType = type; };
protected:
    TYPE mInferredType;
};

/**
 * @brief Node class for integer literals.
 *
 */
class IntASTnode : public ExprAST {
  public:
    IntASTnode(SourceLoc sourceLoc, const TOKEN &tok, int val) : ExprAST(sourceLoc), mVal(val), mToken(tok) {
        mInferredType = TYPE::INT;
    }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

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
    BoolASTnode(SourceLoc sourceLoc, const TOKEN &tok, bool B) : ExprAST(sourceLoc), mBool(B), mToken(tok) {
        mInferredType = TYPE::BOOL;
    }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

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
    FloatASTnode(SourceLoc sourceLoc, const TOKEN &tok, double val) : ExprAST(sourceLoc), mVal(val), mToken(tok) {
        mInferredType = TYPE::FLOAT;
    }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

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
    VariableASTnode(SourceLoc sourceLoc, const TOKEN &tok, const std::string &name)
        : ExprAST(sourceLoc), mToken(tok), mName(name) {}
    const std::string &getName() const { return mName; }
    std::string to_string(int indent = 0) const override;

    Symbol *getResolvedSymbol() const { return mpResolvedSymbol; }
    void setResolvedSymbol(Symbol *symbol) { mpResolvedSymbol = symbol; }
    
    TYPE getType() const {
        assert(mpResolvedSymbol && "Symbol not resolved for variable");
        return mpResolvedSymbol->getType();
    }

    void accept(ASTVisitor &v) override { v.visit(*this); }

  private:
    TOKEN mToken;
    std::string mName;
    Symbol *mpResolvedSymbol;

};

/**
 * @brief Class for assignment expressions.
 *
 */
class AssignExprAST : public ExprAST {
  public:
    AssignExprAST(SourceLoc sourceLoc, std::unique_ptr<VariableASTnode> variable,
                  std::unique_ptr<ExprAST> expression)
        : ExprAST(sourceLoc), mVariable(std::move(variable)), mExpression(std::move(expression)) {}

    std::string to_string(int indent = 0) const override;

    void accept(ASTVisitor &v) override { v.visit(*this); }

    VariableASTnode* getVariable() {return mVariable.get(); }
    ExprAST* getExpression() {return mExpression.get(); }
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
    BinaryExprAST(SourceLoc sourceLoc, std::unique_ptr<ExprAST> left, TOKEN_TYPE op,
                  std::unique_ptr<ExprAST> right)
        : ExprAST(sourceLoc), mLeft(std::move(left)), mOp(op), mRight(std::move(right)) {}
    std::string to_string(int indent = 0) const override;
    
    void accept(ASTVisitor &v) override { v.visit(*this); }

    ExprAST* getLHS() { return mLeft.get(); }
    ExprAST* getRHS() { return mRight.get(); }
    TOKEN_TYPE getOperator() { return mOp; }

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
    UnaryExprAST(SourceLoc sourceLoc, TOKEN_TYPE op, std::unique_ptr<ExprAST> expression)
        : ExprAST(sourceLoc), mOp(op), mExpression(std::move(expression)) {}

    std::string to_string(int indent = 0) const override;

    void accept(ASTVisitor &v) override { v.visit(*this); }

    TOKEN_TYPE getOperator() { return mOp; }
    ExprAST* getExpression() { return mExpression.get(); }

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
    ArgsAST(SourceLoc sourceLoc, std::vector<std::unique_ptr<ExprAST>> args)
        : ExprAST(sourceLoc), ArgsList(std::move(args)) {}

    std::string to_string(int indent = 0) const override;

    void accept(ASTVisitor &v) override { v.visit(*this); }

    std::vector<std::unique_ptr<ExprAST>> &getArgsList() { return ArgsList; }

  private:
    std::vector<std::unique_ptr<ExprAST>> ArgsList;
};

/**
 * @brief Class for a function call expression.
 *
 */
class CallExprAST : public ExprAST {
  public:
    CallExprAST(SourceLoc sourceLoc, std::unique_ptr<VariableASTnode> callee,
                std::unique_ptr<ArgsAST> args)
        : ExprAST(sourceLoc), mCallee(std::move(callee)), mArgs(std::move(args)) {};
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    VariableASTnode* getCallee() { return mCallee.get(); }
    ArgsAST* getArgs() { return mArgs.get(); }

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
    ParamAST(SourceLoc sourceLoc, const std::string &name, TYPE type) : ASTnode(sourceLoc), mName(name), mType(type) {}
    const std::string &getName() const { return mName; }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    Symbol *getSymbol() const { return symbol; }
    void setSymbol(Symbol *sym) { symbol = sym; }

    TYPE getType() const { return mType; }
    void setType(TYPE type) { mType = type; }

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
    VarDeclAST(SourceLoc sourceLoc, const std::string &name, TYPE type) : DeclAST(sourceLoc), mName(name), mType(type) {}
    const std::string &getName() const { return mName; }
    const TYPE getType() const { return mType; }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

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
    GlobVarDeclAST(SourceLoc sourceLoc, const std::string &name, TYPE type)
        : DeclAST(sourceLoc), mName(name), mType(type) {}
    const std::string &getName() const { return mName; }
    const TYPE getType() const { return mType; }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

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
    BlockAST(SourceLoc sourceLoc, std::vector<std::unique_ptr<VarDeclAST>> localDecls,
             std::vector<std::unique_ptr<ASTnode>> stmts)
        : ASTnode(sourceLoc), mLocalDecls(std::move(localDecls)), mStmts(std::move(stmts)) {}
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    std::vector<std::unique_ptr<VarDeclAST>> &getLocalDecls() { return mLocalDecls; }
    std::vector<std::unique_ptr<ASTnode>> &getStmts() { return mStmts; }

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
    FunctionPrototypeAST(SourceLoc sourceLoc, const std::string &name, TYPE type,
                         std::vector<std::unique_ptr<ParamAST>> params)
        : ASTnode(sourceLoc), mName(name), mType(type), mParams(std::move(params)) {}

    const std::string &getName() const { return mName; }
    const TYPE getType() const { return mType; }
    int getSize() const { return mParams.size(); }
    std::vector<std::unique_ptr<ParamAST>> &getParams() { return mParams; }
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

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
    FunctionDeclAST(SourceLoc sourceLoc, std::unique_ptr<FunctionPrototypeAST> mProto,
                    std::unique_ptr<BlockAST> mBlock)
        : DeclAST(sourceLoc), mProto(std::move(mProto)), mBlock(std::move(mBlock)) {}
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    FunctionPrototypeAST* getProto() { return mProto.get(); }
    BlockAST* getBlock() { return mBlock.get(); }

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
    IfExprAST(SourceLoc sourceLoc, std::unique_ptr<ExprAST> condition,
              std::unique_ptr<BlockAST> then, std::unique_ptr<BlockAST> _else)
        : ASTnode(sourceLoc), mCondition(std::move(condition)), mThen(std::move(then)),
          mElse(std::move(_else)) {}
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    ExprAST* getCondition() { return mCondition.get(); }
    BlockAST* getThen() { return mThen.get(); }
    BlockAST* getElse() { return mElse.get(); }

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
    WhileExprAST(SourceLoc sourceLoc, std::unique_ptr<ExprAST> condition,
                 std::unique_ptr<ASTnode> body)
        : ASTnode(sourceLoc), mCondition(std::move(condition)), mBody(std::move(body)) {}
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); };

    ExprAST* getCondition() const {return mCondition.get();}
    ASTnode* getBody() const {return mBody.get(); }

  private:
    std::unique_ptr<ExprAST> mCondition;
    std::unique_ptr<ASTnode> mBody;
};

/**
 * @brief Class for a return statement.
 *
 */
class ReturnAST : public ASTnode {
  public:
    ReturnAST(SourceLoc sourceLoc, std::unique_ptr<ExprAST> expression)
        : ASTnode(sourceLoc), mExpression(std::move(expression)) {}
    std::string to_string(int indent = 0) const override;
    void accept(ASTVisitor &v) override { v.visit(*this); }

    ExprAST* getExpression() const { return mExpression.get(); }

  private:
    std::unique_ptr<ExprAST> mExpression;
};
#endif