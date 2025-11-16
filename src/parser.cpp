#include "../include/parser.h"

void Parser::consumeToken() {
    if (mTokenBuffer.empty()) {
        mCurrentToken = mLexer.getNextToken();
    } else {
        mCurrentToken = mTokenBuffer.front();
        mTokenBuffer.pop_front();
    }
}

void Parser::putBackToken(TOKEN token) { mTokenBuffer.push_front(token); }

/*
// TOKEN CurTok;
// static std::deque<TOKEN> tok_buffer;

// Parse function declaration

// TOKEN getNextToken() {

//   if (tok_buffer.size() == 0)
//     tok_buffer.push_back(gettok());

//   TOKEN temp = tok_buffer.front();
//   tok_buffer.pop_front();

//   return CurTok = temp;
// }

// void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }
*/

static void ReportError(TOKEN tok, const char *Str) {
    throw ParseError(Str, tok.lineNo, tok.columnNo);
}

static void ReportError(const char *Str) { throw ParseError(Str); }

static TYPE stringToType(const std::string &s) {
    if (s == "int")
        return TYPE::INT;
    if (s == "float")
        return TYPE::FLOAT;
    if (s == "bool")
        return TYPE::BOOL;
    ReportError("Invalid type string for variable/function declaration");
}

std::unique_ptr<FloatASTnode> Parser::ParseFloatNumberExpr() {
    auto Result = std::make_unique<FloatASTnode>(mCurrentToken,
                                                 mCurrentToken.getFloatVal());
    consumeToken(); // consume the number
    return std::move(Result);
}

std::unique_ptr<IntASTnode> Parser::ParseIntNumberExpr() {
    auto Result =
        std::make_unique<IntASTnode>(mCurrentToken, mCurrentToken.getIntVal());
    consumeToken(); // consume the number
    return std::move(Result);
}

std::unique_ptr<BoolASTnode> Parser::ParseBoolExpr() {
    auto Result = std::make_unique<BoolASTnode>(mCurrentToken,
                                                mCurrentToken.getBoolVal());
    consumeToken(); // consume the number
    return std::move(Result);
}

std::vector<std::unique_ptr<ParamAST>> Parser::ParseParamListPrime() {
    std::vector<std::unique_ptr<ParamAST>> param_list;

    if (mCurrentToken.type == TOKEN_TYPE::COMMA) { // more parameters in list
        consumeToken();                            // eat ","

        auto param = ParseParam();
        if (param) {
            printf("found param in param_list_prime: %s\n",
                   param->getName().c_str());
            param_list.push_back(std::move(param));
            auto param_list_prime = ParseParamListPrime();
            for (unsigned i = 0; i < param_list_prime.size(); i++) {
                param_list.push_back(std::move(param_list_prime.at(i)));
            }
        }
    } else if (mCurrentToken.type ==
               TOKEN_TYPE::RPAR) { // FOLLOW(param_list_prime)
                                   // expand by param_list_prime ::= ε
                                   // do nothing
    } else {
        ReportError(mCurrentToken,
                    "expected ',' or ')' in list of parameter declarations");
    }

    return param_list;
}

std::unique_ptr<ParamAST> Parser::ParseParam() {

    std::string Type =
        mCurrentToken.lexeme; // keep track of the type of the param
    consumeToken();           // eat the type token
    std::unique_ptr<ParamAST> P;

    if (mCurrentToken.type == TOKEN_TYPE::IDENT) { // parameter declaration
        std::string Name = mCurrentToken.getIdentifierStr();
        consumeToken(); // eat "IDENT"
    }

    return P;
}

std::vector<std::unique_ptr<ParamAST>> Parser::ParseParamList() {
    std::vector<std::unique_ptr<ParamAST>> param_list;

    auto param = ParseParam();
    if (param) {
        param_list.push_back(std::move(param));
        auto param_list_prime = ParseParamListPrime();
        for (unsigned i = 0; i < param_list_prime.size(); i++) {
            param_list.push_back(std::move(param_list_prime.at(i)));
        }
    }

    return param_list;
}

std::vector<std::unique_ptr<ParamAST>> Parser::ParseParams() {
    std::vector<std::unique_ptr<ParamAST>> param_list;

    std::string Type;
    std::string Name = "";

    if (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(param_list)

        auto list = ParseParamList();
        for (unsigned i = 0; i < list.size(); i++) {
            param_list.push_back(std::move(list.at(i)));
        }

    } else if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK) { // FIRST("void")
        // void
        // check that the next token is a )
        consumeToken(); // eat 'void'
        if (mCurrentToken.type != TOKEN_TYPE::RPAR) {
            ReportError(mCurrentToken, "expected ')', after 'void' in \
       end of function declaration");
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::RPAR) { // FOLLOW(params)
        // expand by params ::= ε
        // do nothing
    } else {
        ReportError(
            mCurrentToken,
            "expected 'int', 'bool' or 'float' in function declaration or ') in \
       end of function declaration");
    }

    return param_list;
}

/*** TODO : Task 2 - Parser ***

// args ::= arg_list
//      |  ε
// arg_list ::= arg_list "," expr
//      | expr

// rval ::= rval "||" rval
//      | rval "&&" rval
//      | rval "==" rval | rval "!=" rval
//      | rval "<=" rval | rval "<" rval | rval ">=" rval | rval ">" rval
//      | rval "+" rval | rval "-" rval
//      | rval "*" rval | rval "/" rval | rval "%" rval
//      | "-" rval | "!" rval
//      | "(" expr ")"
//      | IDENT | IDENT "(" args ")"
//      | INT_LIT | FLOAT_LIT | BOOL_LIT
**/

/** ===== NEW =====

// expr              ::= assign
// assign            ::= IDENT "=" assign
//                     | logical_or
// logical_or        ::= logical_and logical_or_prime
// logical_or_prime  ::= "||" logical_and logical_or_prime
//                     | ε
// logical_and       ::= equality logical_and_prime
// logical_and_prime ::= "&&" equality logical_and_prime
//                     | ε
// equality          ::= relational equality_prime
// equality_prime    ::= ("==" | "!=") relational equality_prime
//                     | ε
// relational        ::= additive relational_prime
// relational_prime  ::= ("<" | ">" | "<=" | ">=") additive relational_prime
//                     | ε
// additive          ::= multiplicative additive_prime
// additive_prime    ::= ("+" | "-") multiplicative additive_prime
//                     | ε
// mul               ::= unary mul_prime
// mul_prime         ::= ("*" | "/" | "%") unary mul_prime
//                     | ε
// unary             ::= ("-" | "!") unary
//                     | primary
// primary           ::= IDENT primary_tail
//                     | INT_LIT
//                     | FLOAT_LIT
//                     | BOOL_LIT
//                     | "(" expr ")"
// primary_tail      ::= "(" args ")"
//                     | ε
// args              ::= expr args_tail
//                     | ε
// args_tail         ::= "," expr args_tail
//                     | ε
**/

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
            ReportError(mCurrentToken, "expected expression in function arguments");
        Args.push_back(std::move(Expression));

        if (mCurrentToken.type == TOKEN_TYPE::COMMA) { 
            consumeToken(); // eat ','  
        } else  if (mCurrentToken.type == TOKEN_TYPE::RPAR) { // FOLLOW(args)
            break;
        } else {
            ReportError(mCurrentToken, "expected ',' or ')' in function arguments");
        }
    }

    return Args;
}

std::unique_ptr<ExprAST> Parser::ParsePrimaryTail(
    std::unique_ptr<VariableASTnode> Callee) {
    if (mCurrentToken.type == TOKEN_TYPE::LPAR) {
        consumeToken(); // eat '('
        auto Args = ParseArgs();
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            ReportError(mCurrentToken, "expected ')' after function arguments");
        consumeToken(); // eat ')'
        return std::make_unique<CallExprAST>(std::move(Callee),
                                             std::make_unique<ArgsAST>(std::move(Args)));
    } else {
        // variable reference
        return std::move(Callee);
    }
}

std::unique_ptr<ExprAST> Parser::ParsePrimary() {
    if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
        std::string idName = mCurrentToken.getIdentifierStr();
        auto Variable = std::make_unique<VariableASTnode>(mCurrentToken, idName);
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
            ReportError(mCurrentToken, "expected expression after '('");
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            ReportError(mCurrentToken, "expected ')' after expression");
        consumeToken(); // eat ')'
        return Expression;
    } else {
        ReportError(mCurrentToken, "unknown token when expecting an expression");
    }
}

std::unique_ptr<ExprAST> Parser::ParseUnary() {
    if (mCurrentToken.type == TOKEN_TYPE::MINUS ||
        mCurrentToken.type == TOKEN_TYPE::NOT) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '-' or '!'
        auto Expression = ParseUnary();
        if (!Expression)
            ReportError(mCurrentToken,
                        "expected expression after unary '-' or '!' operator");
        return std::make_unique<UnaryExprAST>(Op, std::move(Expression));
    } else {
        return ParsePrimary();
    }
}

std::unique_ptr<ExprAST> Parser::ParseMultiplicative() {
    auto LHS = ParseUnary();
    if (!LHS)
        ReportError(mCurrentToken, "expected expression in multiplicative");

    while (mCurrentToken.type == TOKEN_TYPE::ASTERIX ||
           mCurrentToken.type == TOKEN_TYPE::DIV ||
           mCurrentToken.type == TOKEN_TYPE::MOD) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '*', '/', or '%'
        auto RHS = ParseUnary();
        if (!RHS)
            ReportError(mCurrentToken,
                        "expected expression after '*', '/', or '%' operator");
        LHS = std::make_unique<BinaryExprAST>(std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseAdditive() {
    auto LHS = ParseMultiplicative();
    if (!LHS)
        ReportError(mCurrentToken, "expected expression in additive");

    while (mCurrentToken.type == TOKEN_TYPE::PLUS ||
           mCurrentToken.type == TOKEN_TYPE::MINUS) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '+' or '-'
        auto RHS = ParseMultiplicative();
        if (!RHS)
            ReportError(mCurrentToken,
                        "expected expression after '+' or '-' operator");
        LHS = std::make_unique<BinaryExprAST>(std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseRelational() {
    auto LHS = ParseAdditive();
    if (!LHS)
        ReportError(mCurrentToken, "expected expression in relational");

    while (mCurrentToken.type == TOKEN_TYPE::LT ||
           mCurrentToken.type == TOKEN_TYPE::GT ||
           mCurrentToken.type == TOKEN_TYPE::LE ||
           mCurrentToken.type == TOKEN_TYPE::GE) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '<', '>', '<=', '>='
        auto RHS = ParseAdditive();
        if (!RHS)
            ReportError(mCurrentToken,
                        "expected expression after relational operator");
        LHS = std::make_unique<BinaryExprAST>(std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseEquality() {
    auto LHS = ParseRelational();
    if (!LHS)
        ReportError(mCurrentToken, "expected expression in equality");

    while (mCurrentToken.type == TOKEN_TYPE::EQ ||
           mCurrentToken.type == TOKEN_TYPE::NE) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '==' or '!='
        auto RHS = ParseRelational();
        if (!RHS)
            ReportError(mCurrentToken, "expected expression after '==' or '!='");
        LHS = std::make_unique<BinaryExprAST>(std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseLogicalAnd() {
    auto LHS = ParseEquality();
    if (!LHS)
        ReportError(mCurrentToken, "expected expression in logical AND");

    while (mCurrentToken.type == TOKEN_TYPE::AND) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '&&'
        auto RHS = ParseEquality();
        if (!RHS)
            ReportError(mCurrentToken, "expected expression after '&&'");
        LHS = std::make_unique<BinaryExprAST>(std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseLogicalOr() {
    auto LHS = ParseLogicalAnd();
    if (!LHS)
        ReportError(mCurrentToken, "expected expression in logical OR");

    while (mCurrentToken.type == TOKEN_TYPE::OR) {
        TOKEN_TYPE Op = mCurrentToken.type;
        consumeToken(); // eat '||'

        auto RHS = ParseLogicalAnd();
        if (!RHS)
            ReportError(mCurrentToken, "expected expression after '||'");

        LHS = std::make_unique<BinaryExprAST>(std::move(LHS), Op, std::move(RHS));
    }

    return LHS;
}

std::unique_ptr<ExprAST> Parser::ParseExper() {
    // Check for assignment
    if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
        TOKEN idToken = mCurrentToken;
        std::string idName = mCurrentToken.getIdentifierStr();
        consumeToken(); // eat IDENT

        if (mCurrentToken.type == TOKEN_TYPE::ASSIGN) {
            consumeToken(); // eat '='

            auto rhs = ParseExper();
            if (!rhs)
                // TODO: this should probably report an error
                ReportError(idToken,
                            "expected expression on right-hand side of assignment");

            return std::make_unique<AssignExprAST>(
                std::make_unique<VariableASTnode>(idToken, idName),
                std::move(rhs));
        } else {
            // Not an assignment, put back the IDENT token and parse as logical_or
            putBackToken(idToken);
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
                ReportError(mCurrentToken,
                            "expected ';' to end expression statement");
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
            ReportError(
                mCurrentToken,
                "expected { to start else block of if-then-else statment");
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
        ReportError(mCurrentToken, "expected 'else' or one of \
    '!', '-', '+', '(' , IDENT , INT_LIT, BOOL_LIT, FLOAT_LIT, ';', \
    '{', 'while', 'if', 'else', ε, 'return', '}' ");

    return nullptr;
}

std::unique_ptr<IfExprAST> Parser::ParseIfStmt() {
    consumeToken(); // eat the if.
    if (mCurrentToken.type == TOKEN_TYPE::LPAR) {
        consumeToken(); // eat (
        // condition.
        auto Cond = ParseExper();
        if (!Cond)
            return nullptr;
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            ReportError(mCurrentToken, "expected )");
        consumeToken(); // eat )

        if (!(mCurrentToken.type == TOKEN_TYPE::LBRA)) {
            ReportError(mCurrentToken,
                        "expected { to start then block of if statment");
        }

        auto Then = ParseBlock();
        if (!Then)
            return nullptr;
        auto Else = ParseElseStmt();

        return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                           std::move(Else));

    } else
        ReportError(mCurrentToken, "expected (");

    return nullptr;
}

std::unique_ptr<ReturnAST> Parser::ParseReturnStmt() {
    consumeToken(); // eat the return
    if (mCurrentToken.type == TOKEN_TYPE::SC) {
        consumeToken(); // eat the ;
        // return a null value
        return std::make_unique<ReturnAST>(std::move(nullptr));
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
            return std::make_unique<ReturnAST>(std::move(val));
        } else
            ReportError(mCurrentToken, "expected ';'");
    } else
        ReportError(mCurrentToken, "expected ';' or expression");

    return nullptr;
}

// while_stmt ::= "while" "(" expr ")" stmt
std::unique_ptr<WhileExprAST> Parser::ParseWhileStmt() {

    consumeToken(); // eat the while.
    if (mCurrentToken.type == TOKEN_TYPE::LPAR) {
        consumeToken(); // eat (
        // condition.
        auto Cond = ParseExper();
        if (!Cond)
            return nullptr;
        if (mCurrentToken.type != TOKEN_TYPE::RPAR)
            ReportError(mCurrentToken, "expected )");
        consumeToken(); // eat )

        auto Body = ParseStmt();
        if (!Body)
            return nullptr;

        return std::make_unique<WhileExprAST>(std::move(Cond), std::move(Body));
    } else
        ReportError(mCurrentToken, "expected (");
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
        fprintf(stderr, "Parsed an expression statement\n");
        return expr_stmt;
    } else if (mCurrentToken.type == TOKEN_TYPE::LBRA) { // FIRST(block)
        auto block_stmt = ParseBlock();
        if (block_stmt) {
            fprintf(stderr, "Parsed a block\n");
            return block_stmt;
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::IF) { // FIRST(if_stmt)
        auto if_stmt = ParseIfStmt();
        if (if_stmt) {
            fprintf(stderr, "Parsed an if statment\n");
            return if_stmt;
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::WHILE) { // FIRST(while_stmt)
        auto while_stmt = ParseWhileStmt();
        if (while_stmt) {
            fprintf(stderr, "Parsed a while statment\n");
            return while_stmt;
        }
    } else if (mCurrentToken.type == TOKEN_TYPE::RETURN) { // FIRST(return_stmt)
        auto return_stmt = ParseReturnStmt();
        if (return_stmt) {
            fprintf(stderr, "Parsed a return statment\n");
            return return_stmt;
        }
    }
    // else if(CurTok.type == RBRA) { // FOLLOW(stmt_list_prime)
    //  expand by stmt_list_prime ::= ε
    //  do nothing
    //}
    else { // syntax error
        ReportError(mCurrentToken, "expected BLA BLA\n");
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

std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDeclsPrime() {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls_prime; // vector of local decls

    if (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(local_decl)
        auto local_decl = ParseLocalDecl();
        if (local_decl) {
            local_decls_prime.push_back(std::move(local_decl));
        }
        auto prime = ParseLocalDeclsPrime();
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
        ReportError(
            mCurrentToken,
            "expected '-', '!', ('' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
      BOOL_LIT, ';', '{', 'if', 'while', 'return' after local variable declaration\n");
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
        consumeToken(); // eat 'int' or 'float or 'bool'
        if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
            Type = PrevTok.lexeme;
            Name = mCurrentToken.getIdentifierStr(); // save the identifier name
            local_decl = std::make_unique<VarDeclAST>(Name, stringToType(Type));

            consumeToken(); // eat 'IDENT'
            if (mCurrentToken.type != TOKEN_TYPE::SC) {
                ReportError(mCurrentToken,
                            "Expected ';' to end local variable declaration");
            }
            consumeToken(); // eat ';'
            fprintf(stderr, "Parsed a local variable declaration\n");
        } else {
            ReportError(mCurrentToken,
                        "expected identifier' in local variable declaration");
        }
    }
    return local_decl;
}

std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDecls() {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls; // vector of local decls

    if (mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(local_decl)

        auto local_decl = ParseLocalDecl();
        if (local_decl) {
            local_decls.push_back(std::move(local_decl));
        }
        auto local_decls_prime = ParseLocalDeclsPrime();
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
        ReportError(
            mCurrentToken,
            "expected '-', '!', '(' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
        BOOL_LIT, ';', '{', 'if', 'while', 'return'");
    }

    return local_decls;
}

std::unique_ptr<BlockAST> Parser::ParseBlock() {
    std::vector<std::unique_ptr<VarDeclAST>>
        local_decls;                                 // vector of local decls
    std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements

    consumeToken(); // eat '{'

    local_decls = ParseLocalDecls();
    fprintf(stderr, "Parsed a set of local variable declaration\n");
    stmt_list = ParseStmtList();
    fprintf(stderr, "Parsed a list of statements\n");
    if (mCurrentToken.type == TOKEN_TYPE::RBRA)
        consumeToken(); // eat '}'
    else {              // syntax error
        ReportError(mCurrentToken, "expected '}' , close body of block");
    }

    return std::make_unique<BlockAST>(std::move(local_decls),
                                      std::move(stmt_list));
}

std::unique_ptr<DeclAST> Parser::ParseDecl() {
    std::string IdName;
    std::vector<std::unique_ptr<ParamAST>> param_list;

    TOKEN PrevTok = mCurrentToken; // to keep track of the type token

    if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
        mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) {
        consumeToken(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

        IdName = mCurrentToken.getIdentifierStr(); // save the identifier name

        if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
            auto ident =
                std::make_unique<VariableASTnode>(mCurrentToken, IdName);
            consumeToken(); // eat the IDENT
            if (mCurrentToken.type ==
                TOKEN_TYPE::SC) { // found ';' then this is a global variable
                                  // declaration.
                consumeToken();   // eat ;
                fprintf(stderr, "Parsed a variable declaration\n");

                if (PrevTok.type != TOKEN_TYPE::VOID_TOK)
                    return std::make_unique<GlobVarDeclAST>(
                        IdName, stringToType(PrevTok.lexeme));
                else
                    ReportError(
                        PrevTok,
                        "Cannot have variable declaration with type 'void'");
            } else if (mCurrentToken.type ==
                       TOKEN_TYPE::LPAR) { // found '(' then this is a function
                                           // declaration.
                consumeToken();            // eat (

                auto P = ParseParams(); // parse the parameters, returns a
                                        // vector of params
                // if (P.size() == 0) return nullptr;
                fprintf(stderr, "Parsed parameter list for function\n");

                if (mCurrentToken.type != TOKEN_TYPE::RPAR) // syntax error
                    ReportError(mCurrentToken,
                                "expected ')' in function declaration");

                consumeToken();                             // eat )
                if (mCurrentToken.type != TOKEN_TYPE::LBRA) // syntax error
                    ReportError(
                        mCurrentToken,
                        "expected '{' in function declaration, function body");

                auto B = ParseBlock(); // parse the function body
                if (!B)
                    return nullptr;
                else
                    fprintf(stderr, "Parsed block of statements in function\n");

                // now create a Function prototype
                // create a Function body
                // put these to together
                // and return a std::unique_ptr<FunctionDeclAST>
                fprintf(stderr, "Parsed a function declaration\n");

                auto Proto = std::make_unique<FunctionPrototypeAST>(
                    IdName, stringToType(PrevTok.lexeme), std::move(P));
                return std::make_unique<FunctionDeclAST>(std::move(Proto),
                                                         std::move(B));
            } else
                ReportError(mCurrentToken, "expected ';' or ('");
        } else
            ReportError(mCurrentToken, "expected an identifier");

    } else
        ReportError(
            mCurrentToken,
            "expected 'void', 'int' or 'float' or EOF token"); // syntax error

    return nullptr;
}

std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclListPrime() {
    // TODO: change return type to vector of unique ptr to DeclAST
    // and implement the collection of decls in this function

    if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
        mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
        mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) { // FIRST(decl)

        if (auto decl = ParseDecl()) {
            fprintf(stderr,
                    "Parsed a top-level variable or function declaration\n");
            // if (auto *DeclIR = decl->codegen()) {
            //   DeclIR->print(errs());
            //   fprintf(stderr, "\n");
            // }
        }
        ParseDeclListPrime();
    } else if (mCurrentToken.type ==
               TOKEN_TYPE::EOF_TOK) { // FOLLOW(decl_list_prime)
                                      // expand by decl_list_prime ::= ε
                                      // do nothing
    } else {                          // syntax error
        ReportError(mCurrentToken,
                    "expected 'void', 'int', 'bool' or 'float' or EOF token");
    }
}

std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclList() {
    auto decl = ParseDecl();
    if (decl) {

        // TODO: remove the code generation code from here
        // TODO: implement the return of vector of decls from this function

        fprintf(stderr,
                "Parsed a top-level variable or function declaration\n");
        // if (auto *DeclIR = decl->codegen()) {
        //   DeclIR->print(errs());
        //   fprintf(stderr, "\n");
        // }
        ParseDeclListPrime();
    }
}

std::unique_ptr<FunctionPrototypeAST> Parser::ParseExtern() {
    std::string IdName;
    TOKEN PrevTok;

    if (mCurrentToken.type == TOKEN_TYPE::EXTERN) {
        consumeToken(); // eat the EXTERN

        if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
            mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
            mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
            mCurrentToken.type == TOKEN_TYPE::BOOL_TOK) {

            PrevTok = mCurrentToken; // to keep track of the type token
            consumeToken(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

            if (mCurrentToken.type == TOKEN_TYPE::IDENT) {
                IdName = mCurrentToken
                             .getIdentifierStr(); // save the identifier name
                auto ident =
                    std::make_unique<VariableASTnode>(mCurrentToken, IdName);
                consumeToken(); // eat the IDENT

                if (mCurrentToken.type ==
                    TOKEN_TYPE::LPAR) { // found '(' - this is an extern
                                        // function declaration.
                    consumeToken(); // eat (

                    auto P = ParseParams(); // parse the parameters, returns a
                                            // vector of params
                    if (P.size() == 0)
                        return nullptr;
                    else
                        fprintf(
                            stderr,
                            "Parsed parameter list for external function\n");

                    if (mCurrentToken.type != TOKEN_TYPE::RPAR) // syntax error
                        ReportError(mCurrentToken,
                                    "expected ')' in closing extern function "
                                    "declaration");

                    consumeToken(); // eat )

                    if (mCurrentToken.type == TOKEN_TYPE::SC) {
                        consumeToken(); // eat ";"
                        auto Proto = std::make_unique<FunctionPrototypeAST>(
                            IdName, stringToType(PrevTok.lexeme), std::move(P));
                        return Proto;
                    } else
                        ReportError(mCurrentToken,
                                    "expected ;' in ending extern function "
                                    "declaration statement");
                } else
                    ReportError(mCurrentToken,
                                "expected (' in extern function declaration");
            }

        } else
            ReportError(mCurrentToken,
                        "expected 'void', 'int' or 'float' in extern function "
                        "declaration\n"); // syntax error
    }

    return nullptr;
}

std::vector<std::unique_ptr<FunctionPrototypeAST>>
Parser::ParseExternListPrime() {

    if (mCurrentToken.type == TOKEN_TYPE::EXTERN) { // FIRST(extern)
        if (auto Extern = ParseExtern()) {
            fprintf(stderr,
                    "Parsed a top-level external function declaration -- 2\n");

            // if (auto *ExternIR = Extern->codegen()) {
            //   ExternIR->print(errs());
            //   fprintf(stderr, "\n");
            // }
        }
        ParseExternListPrime();
    } else if (mCurrentToken.type == TOKEN_TYPE::VOID_TOK ||
               mCurrentToken.type == TOKEN_TYPE::INT_TOK ||
               mCurrentToken.type == TOKEN_TYPE::FLOAT_TOK ||
               mCurrentToken.type ==
                   TOKEN_TYPE::BOOL_TOK) { // FOLLOW(extern_list_prime)
                                           // expand by decl_list_prime ::= ε
                                           // do nothing
    } else {                               // syntax error
        ReportError(mCurrentToken,
                    "expected 'extern' or 'void',  'int' ,  'float',  'bool'");
    }
}

std::vector<std::unique_ptr<FunctionPrototypeAST>> Parser::ParseExternList() {
    auto Extern = ParseExtern();
    if (Extern) {
        fprintf(stderr,
                "Parsed a top-level external function declaration -- 1\n");

        // TODO: remove the code generation code from here
        // if (auto *ExternIR = Extern->codegen()) {
        //   ExternIR->print(errs());
        //   fprintf(stderr, "\n");
        // }
        // fprintf(stderr, "Current token: %s \n", CurTok.lexeme.c_str());
        if (mCurrentToken.type == TOKEN_TYPE::EXTERN)
            ParseExternListPrime();
    }
}

#ifdef USE_PARSER
// program ::= extern_list decl_list
void parser() {
    if (CurTok.type == EOF_TOK)
        return;
    ParseExternList();
    if (CurTok.type == EOF_TOK)
        return;
    ParseDeclList();
    if (CurTok.type == EOF_TOK)
        return;
}
#endif

std::vector<std::unique_ptr<ASTnode>> Parser::parse() {
    std::vector<std::unique_ptr<ASTnode>> decls;

    while (mCurrentToken.type != TOKEN_TYPE::EOF_TOK) {
        if (mCurrentToken.type == TOKEN_TYPE::EXTERN) {
            auto ext = ParseExtern();
            // Note: The current AST structure doesn't handle externs in the
            // main AST vector. This is a design issue in the original project.
            // We'll ignore them for now to get it to link.
        } else {
            auto decl = ParseDecl();
            if (decl) {
                decls.push_back(std::move(decl));
            } else if (mCurrentToken.type != TOKEN_TYPE::EOF_TOK) {
                // If ParseDecl fails, consume a token to avoid an infinite
                // loop.
                consumeToken();
            }
        }
    }
    return decls;
}