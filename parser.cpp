#include "parser.h"

Parser::Parser(Lexer &lexer) : lexer(lexer) {
  // Prime the pump by getting the first token
  consumeToken();
}

void Parser::consumeToken() {
  if (tokenBuffer.empty()) {
    currentToken = lexer.getNextToken();
  } else {
    currentToken = tokenBuffer.front();
    tokenBuffer.pop_front();
  }
}
void Parser::putBackToken(TOKEN token) {
  tokenBuffer.push_front(token);
}

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

[[noreturn]] static void ReportError(TOKEN tok, const char *Str) {
  throw ParseError(Str, tok.mLineNo, tok.mColumnNo);
}

[[noreturn]] static void ReportError(const char *Str) {
  throw ParseError(Str);
}

std::unique_ptr<FloatASTnode> Parser::ParseFloatNumberExpr() {
  auto Result = std::make_unique<FloatASTnode>(currentToken, currentToken.getFloatVal());
  consumeToken(); // consume the number
  return std::move(Result);
}

std::unique_ptr<IntASTnode> Parser::ParseIntNumberExpr() {
  auto Result = std::make_unique<IntASTnode>(currentToken, currentToken.getIntVal());
  consumeToken(); // consume the number
  return std::move(Result);
}

std::unique_ptr<BoolASTnode> Parser::ParseBoolExpr() {
  auto Result = std::make_unique<BoolASTnode>(currentToken, currentToken.getBoolVal());
  consumeToken(); // consume the number
  return std::move(Result);
}

std::vector<std::unique_ptr<ParamAST>> Parser::ParseParamListPrime() {
  std::vector<std::unique_ptr<ParamAST>> param_list;

  if (currentToken.mType == COMMA) { // more parameters in list
    consumeToken();           // eat ","

    auto param = ParseParam();
    if (param) {
      printf("found param in param_list_prime: %s\n", param->getName().c_str());
      param_list.push_back(std::move(param));
      auto param_list_prime = ParseParamListPrime();
      for (unsigned i = 0; i < param_list_prime.size(); i++) {
        param_list.push_back(std::move(param_list_prime.at(i)));
      }
    }
  } else if (currentToken.mType == RPAR) { // FOLLOW(param_list_prime)
    // expand by param_list_prime ::= ε
    // do nothing
  } else {
    ReportError(currentToken, "expected ',' or ')' in list of parameter declarations");
  }

  return param_list;
}

std::unique_ptr<ParamAST> Parser::ParseParam() {

  std::string Type = currentToken.mLexeme; // keep track of the type of the param
  consumeToken();                   // eat the type token
  std::unique_ptr<ParamAST> P;

  if (currentToken.mType == IDENT) { // parameter declaration
    std::string Name = currentToken.getIdentifierStr();
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

  if (currentToken.mType == INT_TOK || currentToken.mType == FLOAT_TOK ||
      currentToken.mType == BOOL_TOK) { // FIRST(param_list)

    auto list = ParseParamList();
    for (unsigned i = 0; i < list.size(); i++) {
      param_list.push_back(std::move(list.at(i)));
    }

  } else if (currentToken.mType == VOID_TOK) { // FIRST("void")
    // void
    // check that the next token is a )
    consumeToken(); // eat 'void'
    if (currentToken.mType != RPAR) {
      ReportError(currentToken, "expected ')', after 'void' in \
       end of function declaration");
    }
  } else if (currentToken.mType == RPAR) { // FOLLOW(params)
    // expand by params ::= ε
    // do nothing
  } else {
    ReportError(
        currentToken,
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

// assign            ::= IDENT "=" assign
//                     | logical_or


std::unique_ptr<ExprAST> Parser::ParseExper() {
  if (currentToken.mType == IDENT) {
    // Go into assignment
    return nullptr;
  }
  return nullptr;
}

std::unique_ptr<ExprAST> Parser::ParseExperStmt() {

  if (currentToken.mType == SC) { // empty statement
    consumeToken();        // eat ;
    return nullptr;
  } else {
    auto expr = ParseExper();
    if (expr) {
      if (currentToken.mType == SC) {
        consumeToken(); // eat ;
        return expr;
      } else {
        ReportError(currentToken, "expected ';' to end expression statement");
      }
    } else
      return nullptr;
  }
  return nullptr;
}

std::unique_ptr<BlockAST> Parser::ParseElseStmt() {

  if (currentToken.mType == ELSE) { // FIRST(else_stmt)
    // expand by else_stmt  ::= "else" "{" stmt "}"
    consumeToken(); // eat "else"

    if (!(currentToken.mType == LBRA)) {
      ReportError(
          currentToken, "expected { to start else block of if-then-else statment");
    }
    auto Else = ParseBlock();
    if (!Else)
      return nullptr;
    return Else;
  } else if (currentToken.mType == NOT || currentToken.mType == MINUS ||
             currentToken.mType == PLUS || currentToken.mType == LPAR ||
             currentToken.mType == IDENT || currentToken.mType == INT_LIT ||
             currentToken.mType == BOOL_LIT || currentToken.mType == FLOAT_LIT ||
             currentToken.mType == SC || currentToken.mType == LBRA || currentToken.mType == WHILE ||
             currentToken.mType == IF || currentToken.mType == ELSE ||
             currentToken.mType == RETURN ||
             currentToken.mType == RBRA) { // FOLLOW(else_stmt)
    // expand by else_stmt  ::= ε
    // return an empty statement
    return nullptr;
  } else
    ReportError(currentToken, "expected 'else' or one of \
    '!', '-', '+', '(' , IDENT , INT_LIT, BOOL_LIT, FLOAT_LIT, ';', \
    '{', 'while', 'if', 'else', ε, 'return', '}' ");

  return nullptr;
}

std::unique_ptr<IfExprAST> Parser::ParseIfStmt() {
  consumeToken(); // eat the if.
  if (currentToken.mType == LPAR) {
    consumeToken(); // eat (
    // condition.
    auto Cond = ParseExper();
    if (!Cond)
      return nullptr;
    if (currentToken.mType != RPAR)
      ReportError(currentToken, "expected )");
    consumeToken(); // eat )

    if (!(currentToken.mType == LBRA)) {
      ReportError(currentToken, "expected { to start then block of if statment");
    }

    auto Then = ParseBlock();
    if (!Then)
      return nullptr;
    auto Else = ParseElseStmt();

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                       std::move(Else));

  } else
    ReportError(currentToken, "expected (");

  return nullptr;
}

std::unique_ptr<ReturnAST> Parser::ParseReturnStmt() {
  consumeToken(); // eat the return
  if (currentToken.mType == SC) {
    consumeToken(); // eat the ;
    // return a null value
    return std::make_unique<ReturnAST>(std::move(nullptr));
  } else if (currentToken.mType == NOT || currentToken.mType == MINUS ||
             currentToken.mType == PLUS || currentToken.mType == LPAR ||
             currentToken.mType == IDENT || currentToken.mType == BOOL_LIT ||
             currentToken.mType == INT_LIT ||
             currentToken.mType == FLOAT_LIT) { // FIRST(expr)
    auto val = ParseExper();
    if (!val)
      return nullptr;

    if (currentToken.mType == SC) {
      consumeToken(); // eat the ;
      return std::make_unique<ReturnAST>(std::move(val));
    } else
      ReportError(currentToken, "expected ';'");
  } else
    ReportError(currentToken, "expected ';' or expression");

  return nullptr;
}

// while_stmt ::= "while" "(" expr ")" stmt
std::unique_ptr<WhileExprAST> Parser::ParseWhileStmt() {

  consumeToken(); // eat the while.
  if (currentToken.mType == LPAR) {
    consumeToken(); // eat (
    // condition.
    auto Cond = ParseExper();
    if (!Cond)
      return nullptr;
    if (currentToken.mType != RPAR)
      ReportError(currentToken, "expected )");
    consumeToken(); // eat )

    auto Body = ParseStmt();
    if (!Body)
      return nullptr;

    return std::make_unique<WhileExprAST>(std::move(Cond), std::move(Body));
  } else
    ReportError(currentToken, "expected (");
}

std::unique_ptr<ASTnode> Parser::ParseStmt() {

  if (currentToken.mType == NOT || currentToken.mType == MINUS || currentToken.mType == PLUS ||
      currentToken.mType == LPAR || currentToken.mType == IDENT || currentToken.mType == BOOL_LIT ||
      currentToken.mType == INT_LIT || currentToken.mType == FLOAT_LIT ||
      currentToken.mType == SC) { // FIRST(expr_stmt)
    // expand by stmt ::= expr_stmt
    auto expr_stmt = ParseExperStmt();
    fprintf(stderr, "Parsed an expression statement\n");
    return expr_stmt;
  } else if (currentToken.mType == LBRA) { // FIRST(block)
    auto block_stmt = ParseBlock();
    if (block_stmt) {
      fprintf(stderr, "Parsed a block\n");
      return block_stmt;
    }
  } else if (currentToken.mType == IF) { // FIRST(if_stmt)
    auto if_stmt = ParseIfStmt();
    if (if_stmt) {
      fprintf(stderr, "Parsed an if statment\n");
      return if_stmt;
    }
  } else if (currentToken.mType == WHILE) { // FIRST(while_stmt)
    auto while_stmt = ParseWhileStmt();
    if (while_stmt) {
      fprintf(stderr, "Parsed a while statment\n");
      return while_stmt;
    }
  } else if (currentToken.mType == RETURN) { // FIRST(return_stmt)
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
    ReportError(currentToken, "expected BLA BLA\n");
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
  if (currentToken.mType == NOT || currentToken.mType == MINUS || currentToken.mType == PLUS ||
      currentToken.mType == LPAR || currentToken.mType == IDENT || currentToken.mType == BOOL_LIT ||
      currentToken.mType == INT_LIT || currentToken.mType == FLOAT_LIT || currentToken.mType == SC ||
      currentToken.mType == LBRA || currentToken.mType == WHILE || currentToken.mType == IF ||
      currentToken.mType == ELSE || currentToken.mType == RETURN) { // FIRST(stmt)
    // expand by stmt_list ::= stmt stmt_list_prime
    auto stmt = ParseStmt();
    if (stmt) {
      stmt_list.push_back(std::move(stmt));
    }
    auto stmt_prime = ParseStmtListPrime();
    for (unsigned i = 0; i < stmt_prime.size(); i++) {
      stmt_list.push_back(std::move(stmt_prime.at(i)));
    }

  } else if (currentToken.mType == RBRA) { // FOLLOW(stmt_list_prime)
    // expand by stmt_list_prime ::= ε
    // do nothing
  }
  return stmt_list; // note stmt_list can be empty as we can have empty blocks,
                    // etc.
}

std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDeclsPrime() {
  std::vector<std::unique_ptr<VarDeclAST>>
      local_decls_prime; // vector of local decls

  if (currentToken.mType == INT_TOK || currentToken.mType == FLOAT_TOK ||
      currentToken.mType == BOOL_TOK) { // FIRST(local_decl)
    auto local_decl = ParseLocalDecl();
    if (local_decl) {
      local_decls_prime.push_back(std::move(local_decl));
    }
    auto prime = ParseLocalDeclsPrime();
    for (unsigned i = 0; i < prime.size(); i++) {
      local_decls_prime.push_back(std::move(prime.at(i)));
    }
  } else if (currentToken.mType == MINUS || currentToken.mType == NOT ||
             currentToken.mType == LPAR || currentToken.mType == IDENT ||
             currentToken.mType == INT_LIT || currentToken.mType == FLOAT_LIT ||
             currentToken.mType == BOOL_LIT || currentToken.mType == SC ||
             currentToken.mType == LBRA || currentToken.mType == IF || currentToken.mType == WHILE ||
             currentToken.mType == RETURN) { // FOLLOW(local_decls_prime)
    // expand by local_decls_prime ::=  ε
    // do nothing;
  } else {
    ReportError(
        currentToken,
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

  if (currentToken.mType == INT_TOK || currentToken.mType == FLOAT_TOK ||
      currentToken.mType == BOOL_TOK) { // FIRST(var_type)
    PrevTok = currentToken;
    consumeToken(); // eat 'int' or 'float or 'bool'
    if (currentToken.mType == IDENT) {
      Type = PrevTok.mLexeme;
      Name = currentToken.getIdentifierStr(); // save the identifier name
      auto ident = std::make_unique<VariableASTnode>(currentToken, Name);
      local_decl = std::make_unique<VarDeclAST>(std::move(ident), Type);

      consumeToken(); // eat 'IDENT'
      if (currentToken.mType != SC) {
        ReportError(currentToken, "Expected ';' to end local variable declaration");
      }
      consumeToken(); // eat ';'
      fprintf(stderr, "Parsed a local variable declaration\n");
    } else {
      ReportError(currentToken, "expected identifier' in local variable declaration");
    }
  }
  return local_decl;
}

std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDecls() {
  std::vector<std::unique_ptr<VarDeclAST>> local_decls; // vector of local decls

  if (currentToken.mType == INT_TOK || currentToken.mType == FLOAT_TOK ||
      currentToken.mType == BOOL_TOK) { // FIRST(local_decl)

    auto local_decl = ParseLocalDecl();
    if (local_decl) {
      local_decls.push_back(std::move(local_decl));
    }
    auto local_decls_prime = ParseLocalDeclsPrime();
    for (unsigned i = 0; i < local_decls_prime.size(); i++) {
      local_decls.push_back(std::move(local_decls_prime.at(i)));
    }

  } else if (currentToken.mType == MINUS || currentToken.mType == NOT ||
             currentToken.mType == LPAR || currentToken.mType == IDENT ||
             currentToken.mType == INT_LIT || currentToken.mType == RETURN ||
             currentToken.mType == FLOAT_LIT || currentToken.mType == BOOL_LIT ||
             currentToken.mType == COMMA || currentToken.mType == LBRA || currentToken.mType == IF ||
             currentToken.mType == WHILE) { // FOLLOW(local_decls)
                                     // do nothing
  } else {
    ReportError(
        currentToken,
        "expected '-', '!', '(' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
        BOOL_LIT, ';', '{', 'if', 'while', 'return'");
  }

  return local_decls;
}

std::unique_ptr<BlockAST> Parser::ParseBlock() {
  std::vector<std::unique_ptr<VarDeclAST>> local_decls; // vector of local decls
  std::vector<std::unique_ptr<ASTnode>> stmt_list;      // vector of statements

  consumeToken(); // eat '{'

  local_decls = ParseLocalDecls();
  fprintf(stderr, "Parsed a set of local variable declaration\n");
  stmt_list = ParseStmtList();
  fprintf(stderr, "Parsed a list of statements\n");
  if (currentToken.mType == RBRA)
    consumeToken(); // eat '}'
  else {            // syntax error
    ReportError(currentToken, "expected '}' , close body of block");
  }

  return std::make_unique<BlockAST>(std::move(local_decls),
                                    std::move(stmt_list));
}


std::unique_ptr<DeclAST> Parser::ParseDecl() {
  std::string IdName;
  std::vector<std::unique_ptr<ParamAST>> param_list;

  TOKEN PrevTok = currentToken; // to keep track of the type token

  if (currentToken.mType == VOID_TOK || currentToken.mType == INT_TOK ||
      currentToken.mType == FLOAT_TOK || currentToken.mType == BOOL_TOK) {
    consumeToken(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

    IdName = currentToken.getIdentifierStr(); // save the identifier name

    if (currentToken.mType == IDENT) {
      auto ident = std::make_unique<VariableASTnode>(currentToken, IdName);
      consumeToken(); // eat the IDENT
      if (currentToken.mType ==
          SC) {         // found ';' then this is a global variable declaration.
        consumeToken(); // eat ;
        fprintf(stderr, "Parsed a variable declaration\n");

        if (PrevTok.mType != VOID_TOK)
          return std::make_unique<GlobVarDeclAST>(std::move(ident),
                                                  PrevTok.mLexeme);
        else
          ReportError(PrevTok,
                          "Cannot have variable declaration with type 'void'");
      } else if (currentToken.mType ==
                 LPAR) { // found '(' then this is a function declaration.
        consumeToken();  // eat (

        auto P =
            ParseParams(); // parse the parameters, returns a vector of params
        // if (P.size() == 0) return nullptr;
        fprintf(stderr, "Parsed parameter list for function\n");

        if (currentToken.mType != RPAR) // syntax error
          ReportError(currentToken, "expected ')' in function declaration");

        consumeToken();          // eat )
        if (currentToken.mType != LBRA) // syntax error
          ReportError(
              currentToken, "expected '{' in function declaration, function body");

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
            IdName, PrevTok.mLexeme, std::move(P));
        return std::make_unique<FunctionDeclAST>(std::move(Proto),
                                                 std::move(B));
      } else
        ReportError(currentToken, "expected ';' or ('");
    } else
      ReportError(currentToken, "expected an identifier");

  } else
    ReportError(currentToken,
             "expected 'void', 'int' or 'float' or EOF token"); // syntax error

  return nullptr;
}

std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclListPrime() {
  // TODO: change return type to vector of unique ptr to DeclAST
  // and implement the collection of decls in this function

  if (currentToken.mType == VOID_TOK || currentToken.mType == INT_TOK ||
      currentToken.mType == FLOAT_TOK || currentToken.mType == BOOL_TOK) { // FIRST(decl)

    if (auto decl = ParseDecl()) {
      fprintf(stderr, "Parsed a top-level variable or function declaration\n");
      // if (auto *DeclIR = decl->codegen()) {
      //   DeclIR->print(errs());
      //   fprintf(stderr, "\n");
      // }
    }
    ParseDeclListPrime();
  } else if (currentToken.mType == EOF_TOK) { // FOLLOW(decl_list_prime)
    // expand by decl_list_prime ::= ε
    // do nothing
  } else { // syntax error
    ReportError(currentToken, "expected 'void', 'int', 'bool' or 'float' or EOF token");
  }
}

std::vector<std::unique_ptr<DeclAST>> Parser::ParseDeclList() {
  auto decl = ParseDecl();
  if (decl) {

    // TODO: remove the code generation code from here
    // TODO: implement the return of vector of decls from this function

    fprintf(stderr, "Parsed a top-level variable or function declaration\n");
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

  if (currentToken.mType == EXTERN) {
    consumeToken(); // eat the EXTERN

    if (currentToken.mType == VOID_TOK || currentToken.mType == INT_TOK ||
        currentToken.mType == FLOAT_TOK || currentToken.mType == BOOL_TOK) {

      PrevTok = currentToken; // to keep track of the type token
      consumeToken();   // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

      if (currentToken.mType == IDENT) {
        IdName = currentToken.getIdentifierStr(); // save the identifier name
        auto ident = std::make_unique<VariableASTnode>(currentToken, IdName);
        consumeToken(); // eat the IDENT

        if (currentToken.mType ==
            LPAR) {       // found '(' - this is an extern function declaration.
          consumeToken(); // eat (

          auto P =
              ParseParams(); // parse the parameters, returns a vector of params
          if (P.size() == 0)
            return nullptr;
          else
            fprintf(stderr, "Parsed parameter list for external function\n");

          if (currentToken.mType != RPAR) // syntax error
            ReportError(
                currentToken, "expected ')' in closing extern function declaration");

          consumeToken(); // eat )

          if (currentToken.mType == SC) {
            consumeToken(); // eat ";"
            auto Proto = std::make_unique<FunctionPrototypeAST>(
                IdName, PrevTok.mLexeme, std::move(P));
            return Proto;
          } else
            ReportError(
                currentToken,
                "expected ;' in ending extern function declaration statement");
        } else
          ReportError(currentToken,
                           "expected (' in extern function declaration");
      }

    } else
      ReportError(currentToken, "expected 'void', 'int' or 'float' in extern function "
                        "declaration\n"); // syntax error
  }

  return nullptr;
}

std::vector<std::unique_ptr<FunctionPrototypeAST>> Parser::ParseExternListPrime() {

  if (currentToken.mType == EXTERN) { // FIRST(extern)
    if (auto Extern = ParseExtern()) {
      fprintf(stderr,
              "Parsed a top-level external function declaration -- 2\n");

      // if (auto *ExternIR = Extern->codegen()) {
      //   ExternIR->print(errs());
      //   fprintf(stderr, "\n");
      // }
    }
    ParseExternListPrime();
  } else if (currentToken.mType == VOID_TOK || currentToken.mType == INT_TOK ||
             currentToken.mType == FLOAT_TOK ||
             currentToken.mType == BOOL_TOK) { // FOLLOW(extern_list_prime)
    // expand by decl_list_prime ::= ε
    // do nothing
  } else { // syntax error
    ReportError(currentToken, "expected 'extern' or 'void',  'int' ,  'float',  'bool'");
  }
}

std::vector<std::unique_ptr<FunctionPrototypeAST>> Parser::ParseExternList() {
  auto Extern = ParseExtern();
  if (Extern) {
    fprintf(stderr, "Parsed a top-level external function declaration -- 1\n");

    // TODO: remove the code generation code from here 
    // if (auto *ExternIR = Extern->codegen()) {
    //   ExternIR->print(errs());
    //   fprintf(stderr, "\n");
    // }
    // fprintf(stderr, "Current token: %s \n", CurTok.lexeme.c_str());
    if (currentToken.mType == EXTERN)
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