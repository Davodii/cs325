#include "ast.h"
#include "lexer.h"
#include "types.h"
#include <string>

// TODO: Provide definitions for the methods that are declared in ast.h but not
// defined.

TYPE ExprAST::getType() { return mType; }
std::string ExprAST::to_string() const { return "ExprAST()"; }

TYPE BinaryExprAST::getType() {
    // This should depend on the operation and operand types.
    // Returning a default to allow linking.

    // TODO: I don't know how type checking should work for this

    if (mLeft->getType() != mRight->getType()) {
        // Handle type mismatch (e.g., promote types, throw error, etc.)
        // For simplicity, we will just return the left type here.
        return mLeft->getType();
    }

    // TODO: Implement proper type checking and promotion rules.
    return mLeft->getType();
}

static std::string operator_to_string(TOKEN_TYPE token) {
    // TODO: why doesn't a switch work here?
    if (token == TOKEN_TYPE::PLUS) {
        return "+";
    } else if (token == TOKEN_TYPE::MINUS) {
        return "-";
    } else if (token == TOKEN_TYPE::ASTERIX) {
        return "*";
    } else if (token == TOKEN_TYPE::DIV) {
        return "/";
    } else if (token == TOKEN_TYPE::LT) {
        return "<";
    } else if (token == TOKEN_TYPE::GT) {
        return ">";
    } else if (token == TOKEN_TYPE::EQ) {
        return "==";
    } else if (token == TOKEN_TYPE::NE) {
        return "!=";
    } else if (token == TOKEN_TYPE::ASSIGN) {
        return "=";
    } else if (token == TOKEN_TYPE::NOT) {
        return "!";
    } else {
        return "UNKNOWN_TOKEN";
    }
}

static std::string typeToString(TYPE type) {
    if (type == TYPE::BOOL) {
        return "BOOL";
    }
    if (type == TYPE::INT) {
        return "INT";
    }
    if (type == TYPE::FLOAT) {
        return "FLOAT";
    }
    if (type == TYPE::VOID) {
        return "VOID";
    }
    return "UNKNOWN_TYPE";
}

std::string BinaryExprAST::to_string() const {
    return "BinaryExprAST(" + mLeft->to_string() + ", " + operator_to_string(mOp) +
           ", " + mRight->to_string() + ")";
}

std::string UnaryExprAST::to_string() const {
    return "UnaryExprAST(" + operator_to_string(mOp) + ", " +
           mExpression->to_string() + ")";
}

std::string ArgsAST::to_string() const {
    std::string s = "ArgsAST([";
    for (const auto &arg : ArgsList) {
        s += arg->to_string() + ", ";
    }

    // Remove trailing comma and space if ArgsList is not empty
    if (!ArgsList.empty()) {
        s.pop_back();
        s.pop_back();
    }
    s += "])";
    return s;
}

std::string CallExprAST::to_string() const {
    return "CallExprAST(" + mCallee->to_string() + ", " + mArgs->to_string() +
           ")";
}

std::string ParamAST::to_string() const {
    return "ParamAST(" + mName + ", " + typeToString(mType) + ")";
}

std::string VarDeclAST::to_string() const {
    return "VarDeclAST(" + mName + ", " + typeToString(mType) + ")";
}

std::string GlobVarDeclAST::to_string() const {
    return "GlobVarDeclAST(" + mName + ", " + typeToString(mType) + ")";
}

std::string BlockAST::to_string() const {
    std::string s = "BlockAST([";
    for (const auto &decl : mLocalDecls) {
        s += decl->to_string() + ", ";
    }
    if (!mLocalDecls.empty()) {
        s.pop_back();
        s.pop_back();
    }
    s += "], [";
    for (const auto &stmt : mStmts) {
        s += stmt->to_string() + ", ";
    }
    if (!mStmts.empty()) {
        s.pop_back();
        s.pop_back();
    }
    s += "])";
    return s;
}

std::string FunctionPrototypeAST::to_string() const {
    std::string s =
        "FunctionPrototypeAST(" + mName + ", " + typeToString(mType) + ", [";
    for (const auto &param : mParams) {
        s += param->to_string() + ", ";
    }
    if (!mParams.empty()) {
        s.pop_back();
        s.pop_back();
    }
    s += "])";
    return s;
}

std::string FunctionDeclAST::to_string() const {
    return "FunctionDeclAST(" + mProto->to_string() + ", " +
           mBlock->to_string() + ")";
}

std::string IfExprAST::to_string() const {
    std::string s =
        "IfExprAST(" + mCondition->to_string() + ", " + mThen->to_string();
    if (mElse) {
        s += ", " + mElse->to_string();
    }
    s += ")";
    return s;
}

std::string WhileExprAST::to_string() const {
    return "WhileExprAST(" + mCondition->to_string() + ", " +
           mBody->to_string() + ")";
}

std::string ReturnAST::to_string() const {
    if (mExpression) {
        return "ReturnAST(" + mExpression->to_string() + ")";
    }
    return "ReturnAST()";
}