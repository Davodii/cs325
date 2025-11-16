#ifndef MC_AST_VISITOR_H
#define MC_AST_VISITOR_H

/// TODO: Forward declare AST nodes here?
#include "ast.h"

class ASTVisitor {
  public:
    virtual ~ASTVisitor() = default;

    virtual void visitNode(ASTnode *node) = 0;
    virtual void visitDecl(DeclAST *node) = 0;
    virtual void visitInt(IntASTnode *node) = 0;
    /// TODO: continue for the other nodes
};

#endif