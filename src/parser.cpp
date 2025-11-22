#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include "source_location.h"
#include <memory>
#include <vector>

void Parser::consumeToken() { mCurrentToken = mLexer.getNextToken(); }

TYPE Parser::stringToType(const std::string &s) const {
    if (s == "int") {
        return TYPE::INT;
    } else if (s == "float") {
        return TYPE::FLOAT;
    } else if (s == "bool") {
        return TYPE::BOOL;
    } else if (s == "void") {
        return TYPE::VOID;
    }
    mErrorReporter.panic(mCurrentToken.loc, "one of: 'int', 'float', 'bool', or 'void'", s);
}

std::unique_ptr<FloatASTnode> Parser::ParseFloatNumberExpr() {
    auto Result = std::make_unique<FloatASTnode>(mCurrentToken.loc, mCurrentToken,
                                                 mCurrentToken.getFloatVal());
    consumeToken(); // consume the number
    return std::move(Result);
}

std::unique_ptr<IntASTnode> Parser::ParseIntNumberExpr() {
    auto Result =
        std::make_unique<IntASTnode>(mCurrentToken.loc, mCurrentToken, mCurrentToken.getIntVal());
    consumeToken(); // consume the number
    return std::move(Result);
}

std::unique_ptr<BoolASTnode> Parser::ParseBoolExpr() {
    auto Result = std::make_unique<BoolASTnode>(mCurrentToken.loc, mCurrentToken,
                                                mCurrentToken.getBoolVal());
    consumeToken(); // consume the number
    return std::move(Result);
}

std::vector<std::unique_ptr<ExprAST>> Parser::ParseArgs() {
    std::vector<std::unique_ptr<ExprAST>> Args;

    if (mCurrentToken.type == TOKEN_TYPE::RPAR) {
        // expand by args ::= ε
        // do nothing
        return Args;
    }

    while (true) {
        auto Expression = ParseExper();
        if (!Expression)
            mErrorReporter.panic(mCurrentToken.loc,
                                "expected expression in function arguments",
                                mCurrentToken.lexeme);
        Args.push_back(std::move(Expression));

        if (mCurrentToken.type == TOKEN_TYPE::COMMA) {
            consumeToken();                                  // eat ','
        } else if (mCurrentToken.type == TOKEN_TYPE::RPAR) { // FOLLOW(args)
            break;
        } else {
            mErrorReporter.panic(mCurrentToken.loc,
                        "',' or ')' in function arguments",
                        mCurrentToken.lexeme);
        }
    }

    return Args;
}

std::unique_ptr<ExprAST>
Parser::ParsePrimaryTail(std::unique_ptr<VariableASTnode> Callee) {
    // Make a copy of the current location
    SourceLoc callLoc = mCurrentToken.loc;
    if (mCurrentToken.type == TOKEN_TYPE::LPAR) {
        consumeToken(); // eat '('
        SourceLoc argsLoc = mCurrentToken.loc;
        auto Args = ParseArgs();
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            mErrorReporter.panic(mCurrentToken.loc, "')' after function arguments", mCurrentToken.lexeme);
        consumeToken(); // eat ')'
        return std::make_unique<CallExprAST>(callLoc,
            std::move(Callee), 
            std::make_unique<ArgsAST>(argsLoc, std::move(Args)));
    } else {
        // variable reference
        return std::move(Callee);
    }
}

std::unique_ptr<ExprAST> Parser::ParsePrimary() {
    if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
        SourceLoc primaryLoc = mCurrentToken.loc;
        std::string idName = mCurrentToken.getIdentifierStr();
        auto Variable =
            std::make_unique<VariableASTnode>(primaryLoc, mCurrentToken, idName);
        consumeToken(); // eat IDENT
        return ParsePrimaryTail(std::move(Variable));
    } else if (mCurrentToken.type == TOKEN_TYPE::INT_LIT) {
        return ParseIntNumberExpr();
    } else if (mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT) {
        return ParseFloatNumberExpr();
    } else if (mCurrentToken.type == TOKEN_TYPE::BOOL_LIT) {
        return ParseBoolExpr();
    } else if (mCurrentToken.type == TOKEN_TYPE::LPAR) {
        consumeToken(); // eat '('
        auto Expression = ParseExper();
        if (!Expression)
            mErrorReporter.panic(mCurrentToken.loc, "expression after '('", mCurrentToken.lexeme);
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            mErrorReporter.panic(mCurrentToken.loc, "')' after expression", mCurrentToken.lexeme);
        consumeToken(); // eat ')'
        return Expression;
    } else {
        mErrorReporter.panic(mCurrentToken.loc, "primary expression (identifier, literal, or '(' expression ')')", mCurrentToken.lexeme);
    }
}

std::unique_ptr<ExprAST> Parser::ParseUnary() {
    if (mCurrentToken.type == TOKEN_TYPE::MINUS ||
        mCurrentToken.type == TOKEN_TYPE::NOT) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '-' or '!'
        auto Expression = ParseUnary();
        if (!Expression)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after unary '-' or '!' operator",
                        mCurrentToken.lexeme);
        return std::make_unique<UnaryExprAST>(opLoc, Op, std::move(Expression));
    } else {
        return ParsePrimary();
    }
}

std::unique_ptr<ExprAST> Parser::ParseMultiplicative() {
    auto LHS = ParseUnary();
    if (!LHS)
        mErrorReporter.panic(mCurrentToken.loc, "expression", mCurrentToken.lexeme);

    while (mCurrentToken.type == TOKEN_TYPE::ASTERIX ||
           mCurrentToken.type == TOKEN_TYPE::DIV ||
           mCurrentToken.type == TOKEN_TYPE::MOD) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '*', '/', or '%'
        auto RHS = ParseUnary();
        if (!RHS)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after '*', '/', or '%' operator",
                        mCurrentToken.lexeme);
        LHS =
            std::make_unique<BinaryExprAST>(opLoc, std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseAdditive() {
    auto LHS = ParseMultiplicative();
    if (!LHS)
        mErrorReporter.panic(mCurrentToken.loc, "expression", mCurrentToken.lexeme);

    while (mCurrentToken.type == TOKEN_TYPE::PLUS ||
           mCurrentToken.type == TOKEN_TYPE::MINUS) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '+' or '-'
        auto RHS = ParseMultiplicative();
        if (!RHS)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after '+' or '-' operator",
                        mCurrentToken.lexeme);
        LHS =
            std::make_unique<BinaryExprAST>(opLoc, std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseRelational() {
    auto LHS = ParseAdditive();
    if (!LHS)
        mErrorReporter.panic(mCurrentToken.loc, "expression", mCurrentToken.lexeme);

    while (mCurrentToken.type == TOKEN_TYPE::LT ||
           mCurrentToken.type == TOKEN_TYPE::GT ||
           mCurrentToken.type == TOKEN_TYPE::LE ||
           mCurrentToken.type == TOKEN_TYPE::GE) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '<', '>', '<=', '>='
        auto RHS = ParseAdditive();
        if (!RHS)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after relational operator",
                        mCurrentToken.lexeme);
        LHS =
            std::make_unique<BinaryExprAST>(opLoc, std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseEquality() {
    auto LHS = ParseRelational();
    if (!LHS)
        mErrorReporter.panic(mCurrentToken.loc, "expression", mCurrentToken.lexeme);

    while (mCurrentToken.type == TOKEN_TYPE::EQ ||
           mCurrentToken.type == TOKEN_TYPE::NE) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '==' or '!='
        auto RHS = ParseRelational();
        if (!RHS)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after '==' or '!=' operator",
                        mCurrentToken.lexeme);
        LHS =
            std::make_unique<BinaryExprAST>(opLoc, std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseLogicalAnd() {
    auto LHS = ParseEquality();
    if (!LHS)
        mErrorReporter.panic(mCurrentToken.loc, "expression", mCurrentToken.lexeme);

    while (mCurrentToken.type == TOKEN_TYPE::AND) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '&&'
        auto RHS = ParseEquality();
        if (!RHS)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after '&&' operator",
                        mCurrentToken.lexeme);
        LHS =
            std::make_unique<BinaryExprAST>(opLoc, std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseLogicalOr() {
    auto LHS = ParseLogicalAnd();
    if (!LHS)
        mErrorReporter.panic(mCurrentToken.loc, "expression", mCurrentToken.lexeme);

    while (mCurrentToken.type == TOKEN_TYPE::OR) {
        TOKEN_TYPE Op = mCurrentToken.type;
        SourceLoc opLoc = mCurrentToken.loc;
        consumeToken(); // eat '||'

        auto RHS = ParseLogicalAnd();
        if (!RHS)
            mErrorReporter.panic(mCurrentToken.loc,
                        "expression after '||' operator",
                        mCurrentToken.lexeme);

        LHS =
            std::make_unique<BinaryExprAST>(opLoc, std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseExper() {
    // Check for assignment
    if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
        TOKEN idToken = mCurrentToken;
        SourceLoc primaryLoc = mCurrentToken.loc;
        TOKEN next = mLexer.peekToken();

        if (next.type == TOKEN_TYPE::ASSIGN) {
            std::string idName = mCurrentToken.getIdentifierStr();
            consumeToken(); // eat IDENT
            consumeToken(); // eat '='

            auto rhs = ParseExper();
            if (!rhs)
                mErrorReporter.panic(idToken.loc,
                    "expression on right-hand side of assignment",
                    idToken.lexeme);

            return std::make_unique<AssignExprAST>(primaryLoc,
                std::make_unique<VariableASTnode>(primaryLoc, idToken, idName),
                std::move(rhs));
        }
    }

    // Parse logical_or expression
    return ParseLogicalOr();
}

std::unique_ptr<ExprAST> Parser::ParseExperStmt() {

    if (mCurrentToken.type == TOKEN_TYPE::SC) { // empty statement
        consumeToken();                         // eat ;
        return nullptr;
    } else {
        auto expr = ParseExper();
        if (expr) {
            if (mCurrentToken.type == TOKEN_TYPE::SC) {
                consumeToken(); // eat ;
                return expr;
            } else {
                mErrorReporter.panic(mCurrentToken.loc,
                            "';' to end expression statement",
                            mCurrentToken.lexeme);
            }
        } else
            return nullptr;
    }
    return nullptr;
}

std::unique_ptr<BlockAST> Parser::ParseElseStmt() {

    if (mCurrentToken.type == TOKEN_TYPE::ELSE) { // FIRST(else_stmt)
        // expand by else_stmt  ::= "else" "{" stmt "}"
        consumeToken(); // eat "else"

        if (!(mCurrentToken.type == TOKEN_TYPE::LBRA)) {
            mErrorReporter.panic(mCurrentToken.loc,
                "'{' to start else block of if-then-else statment",
                mCurrentToken.lexeme);
        }
        auto Else = ParseBlock();
        if (!Else)
            return nullptr;
        return Else;
    } else if (mCurrentToken.type == TOKEN_TYPE::NOT ||
               mCurrentToken.type == TOKEN_TYPE::MINUS ||
               mCurrentToken.type == TOKEN_TYPE::PLUS ||
               mCurrentToken.type == TOKEN_TYPE::LPAR ||
               mCurrentToken.type == TOKEN_TYPE::IDENT ||
               mCurrentToken.type == TOKEN_TYPE::INT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::BOOL_LIT ||
               mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::SC ||
               mCurrentToken.type == TOKEN_TYPE::LBRA ||
               mCurrentToken.type == TOKEN_TYPE::WHILE ||
               mCurrentToken.type == TOKEN_TYPE::IF ||
               mCurrentToken.type == TOKEN_TYPE::ELSE ||
               mCurrentToken.type == TOKEN_TYPE::RETURN ||
               mCurrentToken.type == TOKEN_TYPE::RBRA) { // FOLLOW(else_stmt)
        // expand by else_stmt  ::= ε
        // return an empty statement
        return nullptr;
    } else
        mErrorReporter.panic(mCurrentToken.loc, "'else' or nothing",
    mCurrentToken.lexeme);

    return nullptr;
}

std::unique_ptr<IfExprAST> Parser::ParseIfStmt() {
    if (mCurrentToken.type != TOKEN_TYPE::IF) {
        mErrorReporter.panic(mCurrentToken.loc,
                    "'if' in if statement declaration",
                    mCurrentToken.lexeme);
    }

    SourceLoc ifLoc = mCurrentToken.loc;

    consumeToken(); // eat the if.
    if (mCurrentToken.type == TOKEN_TYPE::LPAR) {
        consumeToken(); // eat (
        // condition.
        auto Cond = ParseExper();
        if (!Cond)
            return nullptr;
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            mErrorReporter.panic(mCurrentToken.loc, "')'", mCurrentToken.lexeme);
        consumeToken(); // eat )

        if (!(mCurrentToken.type == TOKEN_TYPE::LBRA)) {
            mErrorReporter.panic(mCurrentToken.loc,
                        "'{' to start then block of if statment",
                        mCurrentToken.lexeme);
        }

        auto Then = ParseBlock();
        if (!Then)
            return nullptr;
        auto Else = ParseElseStmt();

        return std::make_unique<IfExprAST>(ifLoc, std::move(Cond), std::move(Then),
                                           std::move(Else));

    } else
        mErrorReporter.panic(mCurrentToken.loc, "'('", mCurrentToken.lexeme);

    return nullptr;
}

std::unique_ptr<ReturnAST> Parser::ParseReturnStmt() {
    if (mCurrentToken.type != TOKEN_TYPE::RETURN) {
        mErrorReporter.panic(mCurrentToken.loc,
                    "'return' in return statement declaration",
                    mCurrentToken.lexeme);
    }

    SourceLoc returnLoc = mCurrentToken.loc;

    consumeToken(); // eat the return
    if (mCurrentToken.type == TOKEN_TYPE::SC) {
        consumeToken(); // eat the ;
        // return a null value
        return std::make_unique<ReturnAST>(returnLoc, std::move(nullptr));
    } else if (mCurrentToken.type == TOKEN_TYPE::NOT ||
               mCurrentToken.type == TOKEN_TYPE::MINUS ||
               mCurrentToken.type == TOKEN_TYPE::PLUS ||
               mCurrentToken.type == TOKEN_TYPE::LPAR ||
               mCurrentToken.type == TOKEN_TYPE::IDENT ||
               mCurrentToken.type == TOKEN_TYPE::BOOL_LIT ||
               mCurrentToken.type == TOKEN_TYPE::INT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT) { // FIRST(expr)
        auto val = ParseExper();
        if (!val)
            return nullptr;

        if (mCurrentToken.type == TOKEN_TYPE::SC) {
            consumeToken(); // eat the ;
            return std::make_unique<ReturnAST>(returnLoc, std::move(val));
        } else
            mErrorReporter.panic(mCurrentToken.loc, "';'", mCurrentToken.lexeme);
    } else
        mErrorReporter.panic(mCurrentToken.loc, "';' or expression", mCurrentToken.lexeme);

    return nullptr;
}

std::unique_ptr<WhileExprAST> Parser::ParseWhileStmt() {
    if (mCurrentToken.type != TOKEN_TYPE::WHILE) {
        mErrorReporter.panic(mCurrentToken.loc,
                    "'while' in while statement declaration",
                    mCurrentToken.lexeme);
    }

    SourceLoc whileLoc = mCurrentToken.loc;

    consumeToken(); // eat the while.

    if (mCurrentToken.type != TOKEN_TYPE::LPAR) {
        mErrorReporter.panic(mCurrentToken.loc, "'('", mCurrentToken.lexeme);
    }
    consumeToken(); // eat (

    // condition.
    auto Cond = ParseExper();
    if (!Cond)
        mErrorReporter.panic(mCurrentToken.loc,
                    "expected expression in while statement condition",
                    mCurrentToken.lexeme);

    if (mCurrentToken.type != TOKEN_TYPE::RPAR)
        mErrorReporter.panic(mCurrentToken.loc, "')'", mCurrentToken.lexeme);
    consumeToken(); // eat )

    auto Body = ParseStmt();

    return std::make_unique<WhileExprAST>(whileLoc, std::move(Cond), std::move(Body));
}

std::unique_ptr<ASTnode> Parser::ParseStmt() {

    if (mCurrentToken.type == TOKEN_TYPE::NOT ||
        mCurrentToken.type == TOKEN_TYPE::MINUS ||
        mCurrentToken.type == TOKEN_TYPE::PLUS ||
        mCurrentToken.type == TOKEN_TYPE::LPAR ||
        mCurrentToken.type == TOKEN_TYPE::IDENT ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_LIT ||
        mCurrentToken.type == TOKEN_TYPE::INT_LIT ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT ||
        mCurrentToken.type == TOKEN_TYPE::SC) { // FIRST(expr_stmt)
        // expand by stmt ::= expr_stmt
        auto expr_stmt = ParseExperStmt();
        return expr_stmt;
    } else if (mCurrentToken.type == TOKEN_TYPE::LBRA) { // FIRST(block)
        auto block_stmt = ParseBlock();
        if (block_stmt) {
            return block_stmt;
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::IF) { // FIRST(if_stmt)
        auto if_stmt = ParseIfStmt();
        if (if_stmt) {
            return if_stmt;
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::WHILE) { // FIRST(while_stmt)
        auto while_stmt = ParseWhileStmt();
        if (while_stmt) {
            return while_stmt;
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::RETURN) { // FIRST(return_stmt)
        auto return_stmt = ParseReturnStmt();
        if (return_stmt) {
            return return_stmt;
        }
    }
    // else if(CurTok.type == RBRA) { // FOLLOW(stmt_list_prime)
    //  expand by stmt_list_prime ::= ε
    //  do nothing
    //}
    else { // syntax error
        mErrorReporter.panic(mCurrentToken.loc,
                    "'!', '-', '+', '(' , IDENT , INT_LIT, BOOL_LIT, \
                    FLOAT_LIT, ';', '{', 'while', 'if', 'else', 'return' to \
                    start a statement\n",
                    mCurrentToken.lexeme);
    }
    return nullptr;
}

std::vector<std::unique_ptr<ASTnode>> Parser::ParseStmtList() {
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements
    auto stmt = ParseStmt();
    if (stmt) {
        stmt_list.push_back(std::move(stmt));
    }
    auto stmt_list_prime = ParseStmtListPrime();
    for (unsigned i = 0; i < stmt_list_prime.size(); i++) {
        stmt_list.push_back(std::move(stmt_list_prime.at(i)));
    }
    return stmt_list;
}

std::vector<std::unique_ptr<ASTnode>> Parser::ParseStmtListPrime() {
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements
    if (mCurrentToken.type == TOKEN_TYPE::NOT ||
        mCurrentToken.type == TOKEN_TYPE::MINUS ||
        mCurrentToken.type == TOKEN_TYPE::PLUS ||
        mCurrentToken.type == TOKEN_TYPE::LPAR ||
        mCurrentToken.type == TOKEN_TYPE::IDENT ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_LIT ||
        mCurrentToken.type == TOKEN_TYPE::INT_LIT ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT ||
        mCurrentToken.type == TOKEN_TYPE::SC ||
        mCurrentToken.type == TOKEN_TYPE::LBRA ||
        mCurrentToken.type == TOKEN_TYPE::WHILE ||
        mCurrentToken.type == TOKEN_TYPE::IF ||
        mCurrentToken.type == TOKEN_TYPE::ELSE ||
        mCurrentToken.type == TOKEN_TYPE::RETURN) { // FIRST(stmt)
        // expand by stmt_list ::= stmt stmt_list_prime
        auto stmt = ParseStmt();
        if (stmt) {
            stmt_list.push_back(std::move(stmt));
        }
        auto stmt_prime = ParseStmtListPrime();
        for (unsigned i = 0; i < stmt_prime.size(); i++) {
            stmt_list.push_back(std::move(stmt_prime.at(i)));
        }

    } else if (mCurrentToken.type ==
               TOKEN_TYPE::RBRA) { // FOLLOW(stmt_list_prime)
                                   // expand by stmt_list_prime ::= ε
                                   // do nothing
    }
    return stmt_list; // note stmt_list can be empty as we can have empty
                      // blocks, etc.
}

std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDeclListPrime() {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls_prime; // vector of local decls

    if (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(local_decl)
        auto local_decl = ParseLocalDecl();
        if (local_decl) {
            local_decls_prime.push_back(std::move(local_decl));
        }
        auto prime = ParseLocalDeclListPrime();
        for (unsigned i = 0; i < prime.size(); i++) {
            local_decls_prime.push_back(std::move(prime.at(i)));
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::MINUS ||
               mCurrentToken.type == TOKEN_TYPE::NOT ||
               mCurrentToken.type == TOKEN_TYPE::LPAR ||
               mCurrentToken.type == TOKEN_TYPE::IDENT ||
               mCurrentToken.type == TOKEN_TYPE::INT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::BOOL_LIT ||
               mCurrentToken.type == TOKEN_TYPE::SC ||
               mCurrentToken.type == TOKEN_TYPE::LBRA ||
               mCurrentToken.type == TOKEN_TYPE::IF ||
               mCurrentToken.type == TOKEN_TYPE::WHILE ||
               mCurrentToken.type ==
                   TOKEN_TYPE::RETURN) { // FOLLOW(local_decls_prime)
                                         // expand by local_decls_prime ::=  ε
                                         // do nothing;
    } else {
        mErrorReporter.panic(
            mCurrentToken.loc,
            "'-', '!', ('' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
      BOOL_LIT, ';', '{', 'if', 'while', 'return' after local variable declaration\n",
            mCurrentToken.lexeme);
    }

    return local_decls_prime;
}

std::unique_ptr<VarDeclAST> Parser::ParseLocalDecl() {
    TOKEN PrevTok;
    std::string Type;
    std::string Name = "";
    std::unique_ptr<VarDeclAST> local_decl;

    if (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(var_type)
        PrevTok = mCurrentToken;
        SourceLoc varDeclLoc = mCurrentToken.loc;
        consumeToken(); // eat 'int' or 'float or 'bool'
        if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
            Type = PrevTok.lexeme;
            Name = mCurrentToken.getIdentifierStr(); // save the identifier name
            local_decl = std::make_unique<VarDeclAST>(varDeclLoc, Name, stringToType(Type));

            consumeToken(); // eat 'IDENT'
            if (mCurrentToken.type != TOKEN_TYPE::SC) {
                mErrorReporter.panic(mCurrentToken.loc,
                            "';' to end local variable declaration",
                            mCurrentToken.lexeme);
            }
            consumeToken(); // eat ';'
        } else {
            mErrorReporter.panic(mCurrentToken.loc,
                        "identifier' in local variable declaration",
                        mCurrentToken.lexeme);
        }
    }
    return local_decl;
}

std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDeclList() {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls; // vector of local decls

    if (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(local_decl)

        auto local_decl = ParseLocalDecl();
        if (local_decl) {
            local_decls.push_back(std::move(local_decl));
        }
        auto local_decls_prime = ParseLocalDeclListPrime();
        for (unsigned i = 0; i < local_decls_prime.size(); i++) {
            local_decls.push_back(std::move(local_decls_prime.at(i)));
        }

    } else if (mCurrentToken.type == TOKEN_TYPE::MINUS ||
               mCurrentToken.type == TOKEN_TYPE::NOT ||
               mCurrentToken.type == TOKEN_TYPE::LPAR ||
               mCurrentToken.type == TOKEN_TYPE::IDENT ||
               mCurrentToken.type == TOKEN_TYPE::INT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::RETURN ||
               mCurrentToken.type == TOKEN_TYPE::FLOAT_LIT ||
               mCurrentToken.type == TOKEN_TYPE::BOOL_LIT ||
               mCurrentToken.type == TOKEN_TYPE::COMMA ||
               mCurrentToken.type == TOKEN_TYPE::LBRA ||
               mCurrentToken.type == TOKEN_TYPE::IF ||
               mCurrentToken.type == TOKEN_TYPE::WHILE) { // FOLLOW(local_decls)
                                                          // do nothing
    } else {
        mErrorReporter.panic(
            mCurrentToken.loc,
            "'-', '!', '(' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
        BOOL_LIT, ';', '{', 'if', 'while', 'return'",
            mCurrentToken.lexeme);
    }

    return local_decls;
}

std::unique_ptr<BlockAST> Parser::ParseBlock() {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls;                                 // vector of local decls
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements

    if (mCurrentToken.type != TOKEN_TYPE::LBRA) {
        mErrorReporter.panic(mCurrentToken.loc,
                    "'{' , start body of block statement",
                    mCurrentToken.lexeme);
    }

    SourceLoc blockLoc = mCurrentToken.loc;

    consumeToken(); // eat '{'

    local_decls = ParseLocalDeclList();
    stmt_list = ParseStmtList();
    if (mCurrentToken.type == TOKEN_TYPE::RBRA)
        consumeToken(); // eat '}'
    else {              // syntax error
        mErrorReporter.panic(mCurrentToken.loc,
                    "'}' to end block statement",
                    mCurrentToken.lexeme);
    }

    return std::make_unique<BlockAST>(blockLoc, std::move(local_decls),
                                      std::move(stmt_list));
}

std::vector<std::unique_ptr<ParamAST>> Parser::ParseParamList() {
    std::vector<std::unique_ptr<ParamAST>> param_list;

    std::string Type;
    std::string Name = "";

    // Need to check if there is a parameter or if it's 'void' or empty

    // Parse parameters
    while (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
           mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
           mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(param)
        Type = mCurrentToken.lexeme;
        SourceLoc paramLoc = mCurrentToken.loc;
        consumeToken(); // eat the type token

        if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
            Name = mCurrentToken.getIdentifierStr();
            consumeToken(); // eat IDENT

            auto param = std::make_unique<ParamAST>(paramLoc, Name, stringToType(Type));
            param_list.push_back(std::move(param));

            if (mCurrentToken.type == TOKEN_TYPE::COMMA) {
                consumeToken(); // eat ','
            } else if (mCurrentToken.type ==
                       TOKEN_TYPE::RPAR) { // FOLLOW(param_list)
                break;
            } else {
                mErrorReporter.panic(mCurrentToken.loc,
                            "',' or ')' in function declaration",
                            mCurrentToken.lexeme);
            }
        } else {
            mErrorReporter.panic(mCurrentToken.loc, "identifier in function parameter declaration", mCurrentToken.lexeme);
        }
    }

    if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK) {
        consumeToken(); // eat the 
        if (mCurrentToken.type != TOKEN_TYPE::RPAR) {
        mErrorReporter.panic(mCurrentToken.loc,
                    "')' after 'void' in function declaration",
                    mCurrentToken.lexeme);
        }
    } else if (param_list.size() == 0 &&
               mCurrentToken.type != TOKEN_TYPE::RPAR) {
        mErrorReporter.panic(mCurrentToken.loc, "'void' or parameter list in function declaration", mCurrentToken.lexeme);
    }

    

    return param_list;
}

std::unique_ptr<DeclAST> Parser::ParseDecl() {
    std::string IdName;
    std::vector<std::unique_ptr<ParamAST>> param_list;

    TOKEN typeTok = mCurrentToken; // to keep track of the type token

    if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
        mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) {
        SourceLoc declLoc = mCurrentToken.loc;

        consumeToken(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

        IdName = mCurrentToken.getIdentifierStr(); // save the identifier name

        if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
            consumeToken(); // eat the IDENT
            if (mCurrentToken.type ==
                TOKEN_TYPE::SC) { // found ';' then this is a global variable
                                  // declaration.
                consumeToken();   // eat ;

                if (typeTok.type != TOKEN_TYPE::VOID_TOK)
                    return std::make_unique<GlobVarDeclAST>(declLoc,
                        IdName, stringToType(typeTok.lexeme));
                else
                    mErrorReporter.panic(
                        typeTok.loc,
                        "'int', 'bool' or 'float' type for global variable declaration",
                        typeTok.lexeme);
            } else if (mCurrentToken.type ==
                       TOKEN_TYPE::LPAR) { // found '(' then this is a function
                                           // declaration.
                consumeToken();            // eat (

                auto P = ParseParamList(); // parse the parameters, returns a
                                           // vector of params
                // if (P.size() == 0) return nullptr;

                if (mCurrentToken.type != TOKEN_TYPE::RPAR) // syntax error
                    mErrorReporter.panic(mCurrentToken.loc,
                                "')' in function declaration",
                                mCurrentToken.lexeme);

                consumeToken(); // eat )

                auto Proto = std::make_unique<FunctionPrototypeAST>(declLoc,
                    IdName, stringToType(typeTok.lexeme), std::move(P));

                auto B = ParseBlock(); // parse the function body
                if (!B)
                    mErrorReporter.panic(
                        mCurrentToken.loc,
                        "function body definition",
                        mCurrentToken.lexeme);

                return std::make_unique<FunctionDeclAST>(declLoc, std::move(Proto),
                                                         std::move(B));
            } else
                mErrorReporter.panic(mCurrentToken.loc, "';' or ('", mCurrentToken.lexeme);
        } else
            mErrorReporter.panic(mCurrentToken.loc, "an identifier", mCurrentToken.lexeme);

    } else
        mErrorReporter.panic(
            mCurrentToken.loc,
            "'void', 'int' or 'float' or EOF token",
            mCurrentToken.lexeme); // syntax error

    return nullptr;
}

std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclList() {
    std::vector<std::unique_ptr<DeclAST>> decls; // vector of decls

    // Parse decls
    while (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
           mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
           mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
           mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) {
        auto decl = ParseDecl();
        if (decl) {
            decls.push_back(std::move(decl));
        } else {
            // If ParseDecl fails, consume a token to avoid an infinite loop.
            consumeToken();
        }
    }

    return decls;
}

std::unique_ptr<FunctionPrototypeAST> Parser::ParseExtern() {
    std::string IdName;
    TOKEN PrevTok;

    if (mCurrentToken.type != TOKEN_TYPE::EXTERN) {
        mErrorReporter.panic(mCurrentToken.loc,
                    "'extern' in extern function declaration",
                    mCurrentToken.lexeme);
    }

    consumeToken(); // eat the EXTERN

    if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
        mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) {

        PrevTok = mCurrentToken; // to keep track of the type token
        consumeToken(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

        if (mCurrentToken.type != TOKEN_TYPE::IDENT) {
            mErrorReporter.panic(mCurrentToken.loc,
                        "an identifier in extern function declaration",
                        mCurrentToken.lexeme);
        }

        IdName = mCurrentToken.getIdentifierStr(); // save the identifier name
        SourceLoc funcLoc = mCurrentToken.loc;
        consumeToken();                            // eat the IDENT

        if (mCurrentToken.type != TOKEN_TYPE::LPAR)
            mErrorReporter.panic(mCurrentToken.loc, "'(' in ending extern function "
                                       "statement", mCurrentToken.lexeme);

        consumeToken(); // eat (

        auto P = ParseParamList(); // parse the parameters, returns a
                                   // vector of params

        if (mCurrentToken.type != TOKEN_TYPE::RPAR) // syntax error
            mErrorReporter.panic(mCurrentToken.loc, "')' in closing extern function "
                                       "declaration",
                        mCurrentToken.lexeme);

        consumeToken(); // eat )

        if (mCurrentToken.type == TOKEN_TYPE::SC) {
            consumeToken(); // eat ";"

            auto Proto = std::make_unique<FunctionPrototypeAST>(funcLoc,
                IdName, stringToType(PrevTok.lexeme), std::move(P));
            return std::move(Proto);
        } else
            mErrorReporter.panic(mCurrentToken.loc,
                        "'(' in extern function declaration",
                        mCurrentToken.lexeme);
    } else
        mErrorReporter.panic(mCurrentToken.loc,
                            "'void', 'int', 'bool' or 'float' in extern function declaration",
                            mCurrentToken.lexeme);
}

std::vector<std::unique_ptr<FunctionPrototypeAST>> Parser::ParseExternList() {
    std::vector<std::unique_ptr<FunctionPrototypeAST>>
        externs; // vector of extern function prototypes

    // Parse externs
    while (mCurrentToken.type == TOKEN_TYPE::EXTERN) {
        auto extern_decl = ParseExtern();
        if (extern_decl) {
            externs.push_back(std::move(extern_decl));
        } else {
            // If ParseExtern fails, consume a token to avoid an infinite loop.
            consumeToken();
        }
    }

    return externs;
}

std::unique_ptr<ProgramAST> Parser::parse() {
    std::vector<std::unique_ptr<ASTnode>> externList;
    std::vector<std::unique_ptr<ASTnode>> declarationList;

    SourceLoc programLoc = mCurrentToken.loc;

    if (mCurrentToken.type == TOKEN_TYPE::EOF_TOK)
        return std::make_unique<ProgramAST>(programLoc, std::move(externList), std::move(declarationList));

    auto externs = ParseExternList();
    for (auto &ext : externs) {
        externList.emplace_back(std::move(ext));
    }



    if (mCurrentToken.type == TOKEN_TYPE::EOF_TOK)
        return std::make_unique<ProgramAST>(programLoc, std::move(externList), std::move(declarationList));
    auto decls = ParseDeclList();
    for (auto &decl : decls) {
        declarationList.emplace_back(std::move(decl));
    }

    if (mCurrentToken.type == TOKEN_TYPE::EOF_TOK)
        return std::make_unique<ProgramAST>(programLoc, std::move(externList), std::move(declarationList));
    mErrorReporter.panic(mCurrentToken.loc,
                "EOF token after parsing extern and decl lists",
                mCurrentToken.lexeme);
}