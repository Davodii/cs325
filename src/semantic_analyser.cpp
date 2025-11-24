#include "semantic_analyser.h"
#include "ast.h"
#include "lexer.h"
#include "types.h"
#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

static bool isNumeric(TYPE type) {
    return (type == TYPE::INT || type == TYPE::FLOAT);
}

std::unique_ptr<ProgramAST>
SemanticAnalyser::run(std::unique_ptr<ProgramAST> program) {
    program->accept(*this);
    return program;
}

void SemanticAnalyser::visit(ProgramAST &n) {
    // Iterate over the extern list and visit
    for (auto &ext : *n.getExternList()) {
        ext->accept(*this);
    }

    // Iterate over the declaration list and visit
    for (auto &decl : *n.getDeclarationList()) {
        decl->accept(*this);
    }

    // All nodes should now be visitied
}

void SemanticAnalyser::visit(IntToFloatCastAST &n) {
    // Visit the expression being casted
    n.getExpr()->accept(*this);

    // Check that the expression is of type INT
    if (n.getExpr()->getType() != TYPE::INT) {
        mErrorReporter.typeError(n.getSourceLocation(), "int",
                                 typeToString(n.getExpr()->getType()));
    }

    // The type of this node is already set to FLOAT in the constructor
}

void SemanticAnalyser::visit(IntASTnode &n) {}
void SemanticAnalyser::visit(BoolASTnode &n) {}
void SemanticAnalyser::visit(FloatASTnode &n) {}

void SemanticAnalyser::visit(VariableASTnode &n) {
    // Check the variable has already been declared in a scope
    Symbol *sym = mSymbolTable.lookup(n.getName());

    if (!sym) {
        mErrorReporter.undefinedVariable(n.getSourceLocation(), n.getName());
    }

    // Set the type of the variable
    n.setType(sym->getType());

    // Assign the resolved symbol
    n.setResolvedSymbol(sym);
}

void SemanticAnalyser::visit(AssignExprAST &n) {
    // Visit the variable
    n.getVariable()->accept(*this);

    // Visit the expression
    n.getExpression()->accept(*this);

    // Set the type of the assignment expression
    n.setType(n.getVariable()->getType());

    // Check that the type of the expression is the same
    // as that of the variable
    if (n.getExpression()->getType() == n.getVariable()->getType()) {
        // Everything is correct, can move on
        return;
    }

    // Check if one part of assignment is numeric and the other is boolean
    // If so, there is a type mismatch where we cannot directly convert between
    // types
    if (!(isNumeric(n.getVariable()->getType()) &&
          isNumeric(n.getExpression()->getType()))) {
        mErrorReporter.typeError(n.getSourceLocation(),
                                 typeToString(n.getVariable()->getType()) +
                                     " like variable",
                                 typeToString(n.getExpression()->getType()));
    }

    // There is a type mismatch
    // We only allow type widening (so int to float)
    if (n.getVariable()->getType() == TYPE::INT &&
        n.getExpression()->getType() == TYPE::FLOAT) {
        mErrorReporter.conversionError(
            n.getSourceLocation(), typeToString(n.getVariable()->getType()),
            typeToString(n.getExpression()->getType()));
    }

    // Need to convert the expression to a float
    auto oldNode = n.takeExpression();
    auto castNode = std::make_unique<IntToFloatCastAST>(
        oldNode->getSourceLocation(),
        std::move(oldNode)
    );
    n.setExpression(std::move(castNode));
}

void SemanticAnalyser::visit(BinaryExprAST &n) {
    // Visit the LHS
    n.getLHS()->accept(*this);

    // Visit the RHS
    n.getRHS()->accept(*this);

    // Set the type of the binary expression
    if (n.getOperator() == TOKEN_TYPE::AND ||
        n.getOperator() == TOKEN_TYPE::OR) {
        // Logical operators require boolean operands
        if (n.getLHS()->getType() != TYPE::BOOL) {
            mErrorReporter.typeError(n.getLHS()->getSourceLocation(), "bool",
                                     typeToString(n.getLHS()->getType()));
        }
        if (n.getRHS()->getType() != TYPE::BOOL) {
            mErrorReporter.typeError(n.getRHS()->getSourceLocation(), "bool",
                                     typeToString(n.getRHS()->getType()));
        }
        n.setType(TYPE::BOOL);
    } else if (n.getOperator() == TOKEN_TYPE::EQ ||
               n.getOperator() == TOKEN_TYPE::NE ||
               n.getOperator() == TOKEN_TYPE::LT ||
               n.getOperator() == TOKEN_TYPE::GT ||
               n.getOperator() == TOKEN_TYPE::LE ||
               n.getOperator() == TOKEN_TYPE::GE) {
        // The result of comparison is always boolean
        n.setType(TYPE::BOOL);
    } else {
        // Arithmetic operators
        // Check if either side is a float
        if (n.getLHS()->getType() == TYPE::FLOAT ||
            n.getRHS()->getType() == TYPE::FLOAT) {
            n.setType(TYPE::FLOAT);
        } else {
            n.setType(TYPE::INT);
        }
    }

    // Check the types are the same
    if (n.getLHS()->getType() == n.getRHS()->getType()) {
        return;
    }

    if (!(isNumeric(n.getLHS()->getType()) &&
          isNumeric(n.getRHS()->getType()))) {
        mErrorReporter.typeError(n.getSourceLocation(),
                                 typeToString(n.getLHS()->getType()) +
                                     " like left hand side of expression",
                                 typeToString(n.getRHS()->getType()));
    }

    // There is a type mismatch
    // We only allow type widening (so int to float)
    n.setType(TYPE::FLOAT);

    if (n.getLHS()->getType() == TYPE::INT &&
        n.getRHS()->getType() == TYPE::FLOAT) {
        // Convert the LHS to a float
        auto oldLHS = n.takeLHS();
        auto castNode = std::make_unique<IntToFloatCastAST>(
            oldLHS->getSourceLocation(), 
            std::move(oldLHS)
        );
        n.setLHS(std::move(castNode));
    } else if (n.getLHS()->getType() == TYPE::FLOAT &&
               n.getRHS()->getType() == TYPE::INT) {
        // Convert the RHS to a float
        auto oldRHS = n.takeRHS();
        auto castNode = std::make_unique<IntToFloatCastAST>(
            oldRHS->getSourceLocation(), 
            std::move(oldRHS)
        );
        n.setRHS(std::move(castNode));
    } else {
        // Should not reach here
        throw InternalCompilerError(
            "Unknown type mismatch in binary expression semantic analysis");
    }
}

void SemanticAnalyser::visit(UnaryExprAST &n) {
    // Visit the expression
    n.getExpression()->accept(*this);

    // Check the type of the expression
    TYPE exprType = n.getExpression()->getType();

    if (n.getOperator() == TOKEN_TYPE::MINUS) {
        // Negation operator
        if (isNumeric(exprType)) {
            n.setType(exprType);
            return;
        } else {
            mErrorReporter.typeError(n.getSourceLocation(), "numeric type",
                                     typeToString(exprType));
        }
    } else if (n.getOperator() == TOKEN_TYPE::NOT) {
        // Logical NOT operator
        if (exprType == TYPE::BOOL) {
            n.setType(TYPE::BOOL);
            return;
        } else {
            mErrorReporter.typeError(n.getSourceLocation(), "boolean type",
                                     typeToString(exprType));
        }
    } else {
        // Unknown operator
        throw InternalCompilerError(
            "Unknown unary operator in semantic analysis");
    }
}

void SemanticAnalyser::visit(ArgsAST &n) {
    // Visit all the arguments
    for (auto &arg : n.getArgsList()) {
        arg->accept(*this);
    }
}

void SemanticAnalyser::visit(CallExprAST &n) {
    // Visit the callee
    n.getCallee()->accept(*this);

    // Visit the arguments
    n.getArgs()->accept(*this);

    // Check the callee is a function
    Symbol *sym = n.getCallee()->getResolvedSymbol();
    if (!sym || sym->getKind() != IDENT_TYPE::FUNCTION) {
        mErrorReporter.typeError(n.getCallee()->getSourceLocation(),
                                 "identifier to be a function", "variable");
    }

    // Set the type of the call expression to be that of the function return
    // type
    n.setType(sym->getType());

    FunctionPrototypeAST *proto =
        static_cast<FunctionPrototypeAST *>(sym->getDeclaration());

    // Check the number of arguments matches the number of parameters
    if (n.getArgs()->getArgsList().size() !=
        static_cast<size_t>(proto->getSize())) {
        mErrorReporter.semanticError(
            n.getSourceLocation(),
            std::to_string(proto->getSize()) +
                " number of arguments to match number of parameters, but got " +
                std::to_string(n.getArgs()->getArgsList().size()) +
                " arguments.");
    }

    // Check the types of each argument matches the type of each parameter
    for (size_t i = 0; i < n.getArgs()->getArgsList().size(); ++i) {
        TYPE argType = n.getArgs()->getArgsList()[i]->getType();
        TYPE paramType = proto->getParams()[i]->getType();

        if (argType != paramType) {
            mErrorReporter.typeError(
                n.getArgs()->getArgsList()[i]->getSourceLocation(),
                typeToString(paramType), typeToString(argType));
        }
    }

    // All checks passed
}

void SemanticAnalyser::visit(ParamAST &n) {
    // Add a new entry to symbol table
    Symbol symbol = Symbol(
        n.getName(), n.getType(), IDENT_TYPE::PARAMETER,
        nullptr // TODO: I think the symbol is messed up, this seems wrong
    );

    std::unique_ptr<Symbol> symbolPtr = std::make_unique<Symbol>(symbol);

    // Link the symbol to the parameter AST node
    n.setSymbol(symbolPtr.get());

    if (!mSymbolTable.addSymbol(std::move(symbolPtr))) {
        mErrorReporter.redefinition(n.getSourceLocation(), n.getName());
    }
}

void SemanticAnalyser::visit(VarDeclAST &n) {
    // Check if the variable is already defined in the current scope
    if (mSymbolTable.isDefinedInCurrentScope(n.getName())) {
        mErrorReporter.redefinition(n.getSourceLocation(), n.getName());
    }

    // Add a new entry to symbol table
    Symbol symbol = Symbol(n.getName(), n.getType(), IDENT_TYPE::LOCAL, &n);

    std::unique_ptr<Symbol> symbolPtr = std::make_unique<Symbol>(symbol);

    // Link the symbol to the variable declaration AST node
    n.setSymbol(symbolPtr.get());

    // Add the symbol to the symbol table
    mSymbolTable.addSymbol(std::move(symbolPtr));
}

void SemanticAnalyser::visit(GlobVarDeclAST &n) {
    // Check if the variable is already defined in the current scope
    if (mSymbolTable.isDefinedInCurrentScope(n.getName())) {
        mErrorReporter.redefinition(n.getSourceLocation(), n.getName());
    }

    Symbol symbol = Symbol(n.getName(), n.getType(), IDENT_TYPE::GLOBAL, &n);

    std::unique_ptr<Symbol> symbolPtr = std::make_unique<Symbol>(symbol);

    // Add the symbol to the symbol table
    mSymbolTable.addSymbol(std::move(symbolPtr));

    // Link the symbol to the variable declaration AST node
    n.symbol = symbolPtr.get();
}

void SemanticAnalyser::visit(BlockAST &n) {
    if (n.getLocalDecls().empty() && n.getStmts().empty()) {
        // Empty block, nothing to do
        return;
    }

    if (!n.isFunctionBlock()) {
        // Enter a new scope
        mSymbolTable.enterScope();
    }

    // Visit all local declarations
    for (auto &decl : n.getLocalDecls()) {
        decl->accept(*this);
    }
    // Visit all statements
    for (auto &stmt : n.getStmts()) {
        stmt->accept(*this);
    }

    if (!n.isFunctionBlock()) {
        // Leave the scope
        mSymbolTable.leaveScope();
    }
}

void SemanticAnalyser::visit(FunctionPrototypeAST &n) {
    // Check if the function is already defined in the current scope
    if (mSymbolTable.isDefinedInCurrentScope(n.getName())) {
        mErrorReporter.redefinition(n.getSourceLocation(), n.getName());
    }

    // Add a new entry to symbol table
    Symbol symbol = Symbol(n.getName(), n.getType(), IDENT_TYPE::FUNCTION, &n);

    std::unique_ptr<Symbol> symbolPtr = std::make_unique<Symbol>(symbol);

    // Add the symbol to the symbol table
    // TODO: we need to add this to the global scope, not the current scope
    mSymbolTable.addSymbol(std::move(symbolPtr));

    // Link the symbol to the function prototype AST node
    n.symbol = symbolPtr.get();

    // Enter a new scope
    mSymbolTable.enterScope();

    // Visit the parameters
    for (auto &node : n.getParams()) {
        node->accept(*this);
    }
}

void SemanticAnalyser::visit(FunctionDeclAST &n) {
    // Set the current return type and inside function flag
    mCurrentReturnType = n.getProto()->getType();
    mInsideFunction = true;

    // Visit the prototype
    n.getProto()->accept(*this);

    // Visit the block
    n.getBlock()->accept(*this);

    // End the function scope
    mSymbolTable.leaveScope();

    // Outside of the function
    mInsideFunction = false;
}

void SemanticAnalyser::visit(IfExprAST &n) {
    // Visit the condition
    n.getCondition()->accept(*this);

    // Check if the condition is a boolean
    if (n.getCondition()->getType() != TYPE::BOOL) {
        mErrorReporter.typeError(n.getCondition()->getSourceLocation(), "bool",
                                 typeToString(n.getCondition()->getType()));
    }

    // Visit the then block
    n.getThen()->accept(*this);

    // Visit the else block if it exists
    if (n.getElse()) {
        n.getElse()->accept(*this);
    }
}

void SemanticAnalyser::visit(WhileExprAST &n) {
    // Visit the condition
    n.getCondition()->accept(*this);

    // Check if the condition is a boolean
    if (n.getCondition()->getType() != TYPE::BOOL) {
        mErrorReporter.typeError(n.getCondition()->getSourceLocation(), "bool",
                                 typeToString(n.getCondition()->getType()));
    }

    // Visit the body
    n.getBody()->accept(*this);
}

void SemanticAnalyser::visit(ReturnAST &n) {
    if (!mInsideFunction) {
        mErrorReporter.semanticError(n.getSourceLocation(),
                                     "Return statement not inside a function");
    }

    // Visit the expression (if there is one)
    if (n.getExpression()) {
        // Visit the expression
        n.getExpression()->accept(*this);

        TYPE exprType = n.getExpression()->getType();

        // Check that the expression type matches the current return type
        if (exprType == mCurrentReturnType) {
            // Types match, nothing to do
            return;
        }

        // Check to see if we have a type mismatch between a numeric and boolean
        if (!(isNumeric(exprType) && isNumeric(mCurrentReturnType))) {
            mErrorReporter.typeError(n.getSourceLocation(),
                                     typeToString(mCurrentReturnType),
                                     typeToString(exprType));
        }

        if (mCurrentReturnType == TYPE::FLOAT && exprType == TYPE::INT) {
            // Promote int to a float

            auto oldExpr = n.takeExpression();
            auto castNode = std::make_unique<IntToFloatCastAST>(
                oldExpr->getSourceLocation(), 
                std::move(oldExpr)
            );
            n.setExpression(std::move(castNode));
        } else {
            // Cannot convert float to int
            mErrorReporter.conversionError(n.getSourceLocation(),
                                           typeToString(exprType),
                                           typeToString(mCurrentReturnType));
        }
    } else {
        // We have an empty return statement
        if (mCurrentReturnType != TYPE::VOID) {
            mErrorReporter.semanticError(
                n.getSourceLocation(),
                "returning a value but this function returns nothing.");
        }
    }

    // All checks have passed
}