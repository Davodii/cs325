#include "ast.h"
#include "lexer.h"
#include "types.h"
#include <string>

// TODO: Provide definitions for the methods that are declared in ast.h but not
// defined.

// TYPE ExprAST::getType() { return mType; }

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

static std::string token_to_string(TOKEN_TYPE token) {
    switch (token) {
    case TOKEN_TYPE::PLUS:
        return "+";
    case TOKEN_TYPE::MINUS:
        return "-";
    case TOKEN_TYPE::ASTERIX:
        return "*";
    case TOKEN_TYPE::DIV:
        return "/";
    case TOKEN_TYPE::LT:
        return "<";
    case TOKEN_TYPE::GT:
        return ">";
    case TOKEN_TYPE::EQ:
        return "==";
    case TOKEN_TYPE::NE:
        return "!=";
    case TOKEN_TYPE::ASSIGN:
        return "=";
    case TOKEN_TYPE::NOT:
        return "!";
    default:
        return "UNKNOWN_TOKEN";
    }
}

static std::string type_to_string(TYPE type) {
    switch (type) {
    case TYPE::INT:
        return "INT";
    case TYPE::FLOAT:
        return "FLOAT";
    case TYPE::VOID:
        return "VOID";
    default:
        return "UNKNOWN_TYPE";
    }
}

std::string BinaryExprAST::to_string() const {
    return "BinaryExprAST(" + mLeft->to_string() + ", " + token_to_string(mOp) +
           ", " + mRight->to_string() + ")";
}

std::string UnaryExprAST::to_string() const {
    return "UnaryExprAST(" + token_to_string(mOp) + ", " +
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
    return "ParamAST(" + mName + ", " + type_to_string(mType) + ")";
}

std::string VarDeclAST::to_string() const {
    return "VarDeclAST(" + mName + ", " + type_to_string(mType) + ")";
}

std::string GlobVarDeclAST::to_string() const {
    return "GlobVarDeclAST(" + mName + ", " + type_to_string(mType) + ")";
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
        "FunctionPrototypeAST(" + mName + ", " + type_to_string(mType) + ", [";
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