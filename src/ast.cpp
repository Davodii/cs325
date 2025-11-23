#include "ast.h"

static std::string operator_to_string(TOKEN_TYPE token) {
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

std::string ProgramAST::to_string(int indent) const {
    std::string result = indentString(indent) + "Program:\n";
    result += indentString(indent + 1) + "Externs:\n";
    for (const auto &ext : mExternList) {
        result += ext->to_string(indent + 2) + "\n";
    }
    result += indentString(indent + 1) + "Declarations:\n";
    for (const auto &decl : mDeclarationList) {
        result += decl->to_string(indent + 2) + "\n";
    }
    return result;
};

std::string IntToFloatCastAST::to_string(int indent) const {
    std::string result = indentString(indent) + "Cast Int to Float:\n";
    result += mExpr->to_string(indent + 1);
    return result;
};

std::string IntASTnode::to_string(int indent) const {
    return indentString(indent) + "IntLiteral: " + std::to_string(mVal);
};
std::string BoolASTnode::to_string(int indent) const {
    return indentString(indent) + "BoolLiteral: " + (mBool ? "true" : "false");
};
std::string FloatASTnode::to_string(int indent) const {
    return indentString(indent) + "FloatLiteral: " + std::to_string(mVal);
};
std::string VariableASTnode::to_string(int indent) const {
    return indentString(indent) + "Variable: " + mName;
};
std::string AssignExprAST::to_string(int indent) const {
    std::string result = indentString(indent) + "Assignment:\n";
    result += mVariable->to_string(indent + 1) + "\n";
    result += mExpression->to_string(indent + 1);
    return result;
};
std::string BinaryExprAST::to_string(int indent) const {
    std::string result = indentString(indent) +
                         "BinaryExpression: " + operator_to_string(mOp) + "\n";
    result += mLeft->to_string(indent + 1) + "\n";
    result += mRight->to_string(indent + 1);
    return result;
};
std::string UnaryExprAST::to_string(int indent) const {
    std::string result = indentString(indent) + "UnaryExpression: " +
                         std::to_string(static_cast<int>(mOp)) + "\n";
    result += mExpression->to_string(indent + 1);
    return result;
};
std::string ArgsAST::to_string(int indent) const {
    std::string result = indentString(indent) + "Arguments:\n";
    for (const auto &arg : mArgsList) {
        result += arg->to_string(indent + 1) + "\n";
    }

    // Remove the trailing newline
    if (!mArgsList.empty()) {
        result.pop_back();
    }

    return result;
};
std::string CallExprAST::to_string(int indent) const {
    std::string result = indentString(indent) + "FunctionCall:\n";
    result += mCallee->to_string(indent + 1) + "\n";
    result += mArgs->to_string(indent + 1);
    return result;
};
std::string ParamAST::to_string(int indent) const {
    return indentString(indent) + "Param: " + mName +
           " Type: " + typeToString(mType);
};
std::string VarDeclAST::to_string(int indent) const {
    return indentString(indent) + "VarDecl: " + mName +
           " Type: " + typeToString(mType);
};
std::string GlobVarDeclAST::to_string(int indent) const {
    return indentString(indent) + "GlobVarDecl: " + mName +
           " Type: " + typeToString(mType);
};
std::string BlockAST::to_string(int indent) const {
    std::string result = indentString(indent) + "Block:\n";

    result += indentString(indent + 1) + "Local Declarations:\n";
    for (const auto &decl : mLocalDecls) {
        result += decl->to_string(indent + 2) + "\n";
    }

    result += indentString(indent + 1) + "Statements:\n";
    for (const auto &stmt : mStmts) {
        result += stmt->to_string(indent + 2) + "\n";
    }

    // Remove the trailing newline
    if ((!mLocalDecls.empty()) || (!mStmts.empty())) {
        result.pop_back();
    }

    return result;
};
std::string FunctionPrototypeAST::to_string(int indent) const {
    std::string result = indentString(indent) + "FunctionPrototype: " + mName +
                         " Type: " + typeToString(mType) + "\n";
    result += indentString(indent + 1) + "Parameters:\n";
    for (const auto &param : mParams) {
        result += param->to_string(indent + 2) + "\n";
    }

    // Remove the trailing newline
    if (!mParams.empty()) {
        result.pop_back();
    }

    return result;
};
std::string FunctionDeclAST::to_string(int indent) const {
    std::string result = indentString(indent) + "FunctionDecl:\n";
    result += mProto->to_string(indent + 1) + "\n";
    result += mBlock->to_string(indent + 1);
    return result;
};
std::string IfExprAST::to_string(int indent) const {
    std::string result = indentString(indent) + "IfStatement:\n";
    result += indentString(indent + 1) + "Condition:\n";
    result += mCondition->to_string(indent + 2) + "\n";
    result += indentString(indent + 1) + "Then Block:\n";
    result += mThen->to_string(indent + 2);
    if (mElse) {
        result += "\n" + indentString(indent + 1) + "Else Block:\n";
        result += mElse->to_string(indent + 2);
    }
    return result;
};
std::string WhileExprAST::to_string(int indent) const {
    std::string result = indentString(indent) + "WhileStatement:\n";
    result += indentString(indent + 1) + "Condition:\n";
    result += mCondition->to_string(indent + 2) + "\n";
    result += indentString(indent + 1) + "Body:\n";
    result += mBody->to_string(indent + 2);
    return result;
};
std::string ReturnAST::to_string(int indent) const {
    std::string result = indentString(indent) + "ReturnStatement:\n";
    if (mExpression) {
        result += mExpression->to_string(indent + 1);
    } else {
        result += indentString(indent + 1) + "void";
    }
    return result;
};