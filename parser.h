#ifndef MC_PARSER_H
#define MC_PARSER_H

#include <queue>
#include "lexer.h"
#include "ast.h"

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
extern TOKEN CurTok;

TOKEN getNextToken();
void putBackToken(TOKEN tok);

// TODO: the parser() function is all that is run from the main() function.
// Maybe we can remove the Parse...() functions froms being publicly available.
void parser();

class Parser {
    Lexer &lexer;
    TOKEN currentToken;
    std::deque<TOKEN> tokenBuffer;

    void consumeToken();
    void putBackToken(TOKEN token);

    std::unique_ptr<ASTnode> ParseFloatNumberExpr();
    std::unique_ptr<ASTnode> ParseIntNumberExpr();
    std::unique_ptr<ASTnode> ParseBoolExpr();
    std::unique_ptr<ASTnode> ParseDecl();
    void ParseDeclListPrime();
    void ParseDeclList();
    std::unique_ptr<FunctionPrototypeAST> ParseExtern();
    void ParseExternListPrime();
    void ParseExternList();
    std::unique_ptr<ASTnode> ParseReturnStmt();
    std::unique_ptr<ASTnode> ParseWhileStmt();
    std::unique_ptr<ASTnode> ParseStmt();
    std::vector<std::unique_ptr<ASTnode>> ParseStmtList();
    std::unique_ptr<ASTnode> ParseBlock();
    std::unique_ptr<ASTnode> ParseExper();
    std::unique_ptr<ASTnode> ParseExperStmt();
    std::unique_ptr<ASTnode> ParseElseStmt();
    std::unique_ptr<ASTnode> ParseIfStmt();
    std::vector<std::unique_ptr<ParamAST>> ParseParamListPrime();
    std::unique_ptr<ParamAST> ParseParam();
    std::vector<std::unique_ptr<ParamAST>> ParseParamList();
    std::vector<std::unique_ptr<ParamAST>> ParseParams();
    std::unique_ptr<VarDeclAST> ParseLocalDecl();
    std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDecls();
    std::vector<std::unique_ptr<ASTnode>> ParseStmtListPrime();

    std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDeclsPrime();

public:
    Parser(Lexer &lexer);

    std::vector<std::unique_ptr<ASTnode>> parse();
};

#endif