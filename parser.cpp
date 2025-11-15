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

/// LogError* - These are little helper function for error handling.
std::unique_ptr<ASTnode> LogError(TOKEN tok, const char *Str) {
  fprintf(stderr, "%d:%d Error: %s\n", tok.mLineNo, tok.mColumnNo, Str);
  exit(2);
  return nullptr;
}

std::unique_ptr<FunctionPrototypeAST> LogErrorP(TOKEN tok, const char *Str) {
  LogError(tok, Str);
  exit(2);
  return nullptr;
}

std::unique_ptr<ASTnode> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  exit(2);
  return nullptr;
}

// element ::= FLOAT_LIT
std::unique_ptr<ASTnode> Parser::ParseFloatNumberExpr() {
  auto Result = std::make_unique<FloatASTnode>(CurTok, CurTok.getFloatVal());
  getNextToken(); // consume the number
  return std::move(Result);
}

// element ::= INT_LIT
std::unique_ptr<ASTnode> Parser::ParseIntNumberExpr() {
  auto Result = std::make_unique<IntASTnode>(CurTok, CurTok.getIntVal());
  getNextToken(); // consume the number
  return std::move(Result);
}

// element ::= BOOL_LIT
std::unique_ptr<ASTnode> Parser::ParseBoolExpr() {
  auto Result = std::make_unique<BoolASTnode>(CurTok, CurTok.getBoolVal());
  getNextToken(); // consume the number
  return std::move(Result);
}

// param_list_prime ::= "," param param_list_prime
//                   |  ε
std::vector<std::unique_ptr<ParamAST>> Parser::ParseParamListPrime() {
  std::vector<std::unique_ptr<ParamAST>> param_list;

  if (CurTok.mType == COMMA) { // more parameters in list
    getNextToken();           // eat ","

    auto param = ParseParam();
    if (param) {
      printf("found param in param_list_prime: %s\n", param->getName().c_str());
      param_list.push_back(std::move(param));
      auto param_list_prime = ParseParamListPrime();
      for (unsigned i = 0; i < param_list_prime.size(); i++) {
        param_list.push_back(std::move(param_list_prime.at(i)));
      }
    }
  } else if (CurTok.mType == RPAR) { // FOLLOW(param_list_prime)
    // expand by param_list_prime ::= ε
    // do nothing
  } else {
    LogError(CurTok, "expected ',' or ')' in list of parameter declarations");
  }

  return param_list;
}

// param ::= var_type IDENT
std::unique_ptr<ParamAST> Parser::ParseParam() {

  std::string Type = CurTok.mLexeme; // keep track of the type of the param
  getNextToken();                   // eat the type token
  std::unique_ptr<ParamAST> P;

  if (CurTok.mType == IDENT) { // parameter declaration
    std::string Name = CurTok.getIdentifierStr();
    getNextToken(); // eat "IDENT"
  }

  return P;
}

// param_list ::= param param_list_prime
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

// params ::= param_list
//         |  ε
std::vector<std::unique_ptr<ParamAST>> Parser::ParseParams() {
  std::vector<std::unique_ptr<ParamAST>> param_list;

  std::string Type;
  std::string Name = "";

  if (CurTok.mType == INT_TOK || CurTok.mType == FLOAT_TOK ||
      CurTok.mType == BOOL_TOK) { // FIRST(param_list)

    auto list = ParseParamList();
    for (unsigned i = 0; i < list.size(); i++) {
      param_list.push_back(std::move(list.at(i)));
    }

  } else if (CurTok.mType == VOID_TOK) { // FIRST("void")
    // void
    // check that the next token is a )
    getNextToken(); // eat 'void'
    if (CurTok.mType != RPAR) {
      LogError(CurTok, "expected ')', after 'void' in \
       end of function declaration");
    }
  } else if (CurTok.mType == RPAR) { // FOLLOW(params)
    // expand by params ::= ε
    // do nothing
  } else {
    LogError(
        CurTok,
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


// expr ::= IDENT "=" expr
//      |  rval
std::unique_ptr<ASTnode> Parser::ParseExper() {
  if (CurTok.mType == IDENT) {
    // Go into assignment
    return nullptr;
  }
  return nullptr;
}

// expr_stmt ::= expr ";"
//            |  ";"
std::unique_ptr<ASTnode> Parser::ParseExperStmt() {

  if (CurTok.mType == SC) { // empty statement
    getNextToken();        // eat ;
    return nullptr;
  } else {
    auto expr = ParseExper();
    if (expr) {
      if (CurTok.mType == SC) {
        getNextToken(); // eat ;
        return expr;
      } else {
        LogError(CurTok, "expected ';' to end expression statement");
      }
    } else
      return nullptr;
  }
  return nullptr;
}

// else_stmt  ::= "else" block
//             |  ε
std::unique_ptr<ASTnode> Parser::ParseElseStmt() {

  if (CurTok.mType == ELSE) { // FIRST(else_stmt)
    // expand by else_stmt  ::= "else" "{" stmt "}"
    getNextToken(); // eat "else"

    if (!(CurTok.mType == LBRA)) {
      return LogError(
          CurTok, "expected { to start else block of if-then-else statment");
    }
    auto Else = ParseBlock();
    if (!Else)
      return nullptr;
    return Else;
  } else if (CurTok.mType == NOT || CurTok.mType == MINUS ||
             CurTok.mType == PLUS || CurTok.mType == LPAR ||
             CurTok.mType == IDENT || CurTok.mType == INT_LIT ||
             CurTok.mType == BOOL_LIT || CurTok.mType == FLOAT_LIT ||
             CurTok.mType == SC || CurTok.mType == LBRA || CurTok.mType == WHILE ||
             CurTok.mType == IF || CurTok.mType == ELSE ||
             CurTok.mType == RETURN ||
             CurTok.mType == RBRA) { // FOLLOW(else_stmt)
    // expand by else_stmt  ::= ε
    // return an empty statement
    return nullptr;
  } else
    LogError(CurTok, "expected 'else' or one of \
    '!', '-', '+', '(' , IDENT , INT_LIT, BOOL_LIT, FLOAT_LIT, ';', \
    '{', 'while', 'if', 'else', ε, 'return', '}' ");

  return nullptr;
}

// if_stmt ::= "if" "(" expr ")" block else_stmt
std::unique_ptr<ASTnode> Parser::ParseIfStmt() {
  getNextToken(); // eat the if.
  if (CurTok.mType == LPAR) {
    getNextToken(); // eat (
    // condition.
    auto Cond = ParseExper();
    if (!Cond)
      return nullptr;
    if (CurTok.mType != RPAR)
      return LogError(CurTok, "expected )");
    getNextToken(); // eat )

    if (!(CurTok.mType == LBRA)) {
      return LogError(CurTok, "expected { to start then block of if statment");
    }

    auto Then = ParseBlock();
    if (!Then)
      return nullptr;
    auto Else = ParseElseStmt();

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                       std::move(Else));

  } else
    return LogError(CurTok, "expected (");

  return nullptr;
}

// return_stmt ::= "return" ";"
//             |  "return" expr ";"
std::unique_ptr<ASTnode> Parser::ParseReturnStmt() {
  getNextToken(); // eat the return
  if (CurTok.mType == SC) {
    getNextToken(); // eat the ;
    // return a null value
    return std::make_unique<ReturnAST>(std::move(nullptr));
  } else if (CurTok.mType == NOT || CurTok.mType == MINUS ||
             CurTok.mType == PLUS || CurTok.mType == LPAR ||
             CurTok.mType == IDENT || CurTok.mType == BOOL_LIT ||
             CurTok.mType == INT_LIT ||
             CurTok.mType == FLOAT_LIT) { // FIRST(expr)
    auto val = ParseExper();
    if (!val)
      return nullptr;

    if (CurTok.mType == SC) {
      getNextToken(); // eat the ;
      return std::make_unique<ReturnAST>(std::move(val));
    } else
      return LogError(CurTok, "expected ';'");
  } else
    return LogError(CurTok, "expected ';' or expression");

  return nullptr;
}

// while_stmt ::= "while" "(" expr ")" stmt
std::unique_ptr<ASTnode> Parser::ParseWhileStmt() {

  getNextToken(); // eat the while.
  if (CurTok.mType == LPAR) {
    getNextToken(); // eat (
    // condition.
    auto Cond = ParseExper();
    if (!Cond)
      return nullptr;
    if (CurTok.mType != RPAR)
      return LogError(CurTok, "expected )");
    getNextToken(); // eat )

    auto Body = ParseStmt();
    if (!Body)
      return nullptr;

    return std::make_unique<WhileExprAST>(std::move(Cond), std::move(Body));
  } else
    return LogError(CurTok, "expected (");
}

// stmt ::= expr_stmt
//      |  block
//      |  if_stmt
//      |  while_stmt
//      |  return_stmt
std::unique_ptr<ASTnode> Parser::ParseStmt() {

  if (CurTok.mType == NOT || CurTok.mType == MINUS || CurTok.mType == PLUS ||
      CurTok.mType == LPAR || CurTok.mType == IDENT || CurTok.mType == BOOL_LIT ||
      CurTok.mType == INT_LIT || CurTok.mType == FLOAT_LIT ||
      CurTok.mType == SC) { // FIRST(expr_stmt)
    // expand by stmt ::= expr_stmt
    auto expr_stmt = ParseExperStmt();
    fprintf(stderr, "Parsed an expression statement\n");
    return expr_stmt;
  } else if (CurTok.mType == LBRA) { // FIRST(block)
    auto block_stmt = ParseBlock();
    if (block_stmt) {
      fprintf(stderr, "Parsed a block\n");
      return block_stmt;
    }
  } else if (CurTok.mType == IF) { // FIRST(if_stmt)
    auto if_stmt = ParseIfStmt();
    if (if_stmt) {
      fprintf(stderr, "Parsed an if statment\n");
      return if_stmt;
    }
  } else if (CurTok.mType == WHILE) { // FIRST(while_stmt)
    auto while_stmt = ParseWhileStmt();
    if (while_stmt) {
      fprintf(stderr, "Parsed a while statment\n");
      return while_stmt;
    }
  } else if (CurTok.mType == RETURN) { // FIRST(return_stmt)
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
    return LogError(CurTok, "expected BLA BLA\n");
  }
  return nullptr;
}

// stmt_list ::= stmt stmt_list_prime
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

// stmt_list_prime ::= stmt stmt_list_prime
//                  |  ε
std::vector<std::unique_ptr<ASTnode>> Parser::ParseStmtListPrime() {
  std::vector<std::unique_ptr<ASTnode>> stmt_list; // vector of statements
  if (CurTok.mType == NOT || CurTok.mType == MINUS || CurTok.mType == PLUS ||
      CurTok.mType == LPAR || CurTok.mType == IDENT || CurTok.mType == BOOL_LIT ||
      CurTok.mType == INT_LIT || CurTok.mType == FLOAT_LIT || CurTok.mType == SC ||
      CurTok.mType == LBRA || CurTok.mType == WHILE || CurTok.mType == IF ||
      CurTok.mType == ELSE || CurTok.mType == RETURN) { // FIRST(stmt)
    // expand by stmt_list ::= stmt stmt_list_prime
    auto stmt = ParseStmt();
    if (stmt) {
      stmt_list.push_back(std::move(stmt));
    }
    auto stmt_prime = ParseStmtListPrime();
    for (unsigned i = 0; i < stmt_prime.size(); i++) {
      stmt_list.push_back(std::move(stmt_prime.at(i)));
    }

  } else if (CurTok.mType == RBRA) { // FOLLOW(stmt_list_prime)
    // expand by stmt_list_prime ::= ε
    // do nothing
  }
  return stmt_list; // note stmt_list can be empty as we can have empty blocks,
                    // etc.
}

// local_decls_prime ::= local_decl local_decls_prime
//                    |  ε
std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDeclsPrime() {
  std::vector<std::unique_ptr<VarDeclAST>>
      local_decls_prime; // vector of local decls

  if (CurTok.mType == INT_TOK || CurTok.mType == FLOAT_TOK ||
      CurTok.mType == BOOL_TOK) { // FIRST(local_decl)
    auto local_decl = ParseLocalDecl();
    if (local_decl) {
      local_decls_prime.push_back(std::move(local_decl));
    }
    auto prime = ParseLocalDeclsPrime();
    for (unsigned i = 0; i < prime.size(); i++) {
      local_decls_prime.push_back(std::move(prime.at(i)));
    }
  } else if (CurTok.mType == MINUS || CurTok.mType == NOT ||
             CurTok.mType == LPAR || CurTok.mType == IDENT ||
             CurTok.mType == INT_LIT || CurTok.mType == FLOAT_LIT ||
             CurTok.mType == BOOL_LIT || CurTok.mType == SC ||
             CurTok.mType == LBRA || CurTok.mType == IF || CurTok.mType == WHILE ||
             CurTok.mType == RETURN) { // FOLLOW(local_decls_prime)
    // expand by local_decls_prime ::=  ε
    // do nothing;
  } else {
    LogError(
        CurTok,
        "expected '-', '!', ('' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
      BOOL_LIT, ';', '{', 'if', 'while', 'return' after local variable declaration\n");
  }

  return local_decls_prime;
}

// local_decl ::= var_type IDENT ";"
// var_type ::= "int"
//           |  "float"
//           |  "bool"
std::unique_ptr<VarDeclAST> Parser::ParseLocalDecl() {
  TOKEN PrevTok;
  std::string Type;
  std::string Name = "";
  std::unique_ptr<VarDeclAST> local_decl;

  if (CurTok.mType == INT_TOK || CurTok.mType == FLOAT_TOK ||
      CurTok.mType == BOOL_TOK) { // FIRST(var_type)
    PrevTok = CurTok;
    getNextToken(); // eat 'int' or 'float or 'bool'
    if (CurTok.mType == IDENT) {
      Type = PrevTok.mLexeme;
      Name = CurTok.getIdentifierStr(); // save the identifier name
      auto ident = std::make_unique<VariableASTnode>(CurTok, Name);
      local_decl = std::make_unique<VarDeclAST>(std::move(ident), Type);

      getNextToken(); // eat 'IDENT'
      if (CurTok.mType != SC) {
        LogError(CurTok, "Expected ';' to end local variable declaration");
        return nullptr;
      }
      getNextToken(); // eat ';'
      fprintf(stderr, "Parsed a local variable declaration\n");
    } else {
      LogError(CurTok, "expected identifier' in local variable declaration");
      return nullptr;
    }
  }
  return local_decl;
}

// local_decls ::= local_decl local_decls_prime
std::vector<std::unique_ptr<VarDeclAST>> Parser::ParseLocalDecls() {
  std::vector<std::unique_ptr<VarDeclAST>> local_decls; // vector of local decls

  if (CurTok.mType == INT_TOK || CurTok.mType == FLOAT_TOK ||
      CurTok.mType == BOOL_TOK) { // FIRST(local_decl)

    auto local_decl = ParseLocalDecl();
    if (local_decl) {
      local_decls.push_back(std::move(local_decl));
    }
    auto local_decls_prime = ParseLocalDeclsPrime();
    for (unsigned i = 0; i < local_decls_prime.size(); i++) {
      local_decls.push_back(std::move(local_decls_prime.at(i)));
    }

  } else if (CurTok.mType == MINUS || CurTok.mType == NOT ||
             CurTok.mType == LPAR || CurTok.mType == IDENT ||
             CurTok.mType == INT_LIT || CurTok.mType == RETURN ||
             CurTok.mType == FLOAT_LIT || CurTok.mType == BOOL_LIT ||
             CurTok.mType == COMMA || CurTok.mType == LBRA || CurTok.mType == IF ||
             CurTok.mType == WHILE) { // FOLLOW(local_decls)
                                     // do nothing
  } else {
    LogError(
        CurTok,
        "expected '-', '!', '(' , IDENT , STRING_LIT , INT_LIT , FLOAT_LIT, \
        BOOL_LIT, ';', '{', 'if', 'while', 'return'");
  }

  return local_decls;
}

// parse block
// block ::= "{" local_decls stmt_list "}"
std::unique_ptr<ASTnode> Parser::ParseBlock() {
  std::vector<std::unique_ptr<VarDeclAST>> local_decls; // vector of local decls
  std::vector<std::unique_ptr<ASTnode>> stmt_list;      // vector of statements

  getNextToken(); // eat '{'

  local_decls = ParseLocalDecls();
  fprintf(stderr, "Parsed a set of local variable declaration\n");
  stmt_list = ParseStmtList();
  fprintf(stderr, "Parsed a list of statements\n");
  if (CurTok.mType == RBRA)
    getNextToken(); // eat '}'
  else {            // syntax error
    LogError(CurTok, "expected '}' , close body of block");
    return nullptr;
  }

  return std::make_unique<BlockAST>(std::move(local_decls),
                                    std::move(stmt_list));
}

// decl ::= var_decl
//       |  fun_decl
std::unique_ptr<ASTnode> Parser::ParseDecl() {
  std::string IdName;
  std::vector<std::unique_ptr<ParamAST>> param_list;

  TOKEN PrevTok = CurTok; // to keep track of the type token

  if (CurTok.mType == VOID_TOK || CurTok.mType == INT_TOK ||
      CurTok.mType == FLOAT_TOK || CurTok.mType == BOOL_TOK) {
    getNextToken(); // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

    IdName = CurTok.getIdentifierStr(); // save the identifier name

    if (CurTok.mType == IDENT) {
      auto ident = std::make_unique<VariableASTnode>(CurTok, IdName);
      getNextToken(); // eat the IDENT
      if (CurTok.mType ==
          SC) {         // found ';' then this is a global variable declaration.
        getNextToken(); // eat ;
        fprintf(stderr, "Parsed a variable declaration\n");

        if (PrevTok.mType != VOID_TOK)
          return std::make_unique<GlobVarDeclAST>(std::move(ident),
                                                  PrevTok.mLexeme);
        else
          return LogError(PrevTok,
                          "Cannot have variable declaration with type 'void'");
      } else if (CurTok.mType ==
                 LPAR) { // found '(' then this is a function declaration.
        getNextToken();  // eat (

        auto P =
            ParseParams(); // parse the parameters, returns a vector of params
        // if (P.size() == 0) return nullptr;
        fprintf(stderr, "Parsed parameter list for function\n");

        if (CurTok.mType != RPAR) // syntax error
          return LogError(CurTok, "expected ')' in function declaration");

        getNextToken();          // eat )
        if (CurTok.mType != LBRA) // syntax error
          return LogError(
              CurTok, "expected '{' in function declaration, function body");

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
        return LogError(CurTok, "expected ';' or ('");
    } else
      return LogError(CurTok, "expected an identifier");

  } else
    LogError(CurTok,
             "expected 'void', 'int' or 'float' or EOF token"); // syntax error

  return nullptr;
}

// decl_list_prime ::= decl decl_list_prime
//                  |  ε
void Parser::ParseDeclListPrime() {
  if (CurTok.mType == VOID_TOK || CurTok.mType == INT_TOK ||
      CurTok.mType == FLOAT_TOK || CurTok.mType == BOOL_TOK) { // FIRST(decl)

    if (auto decl = ParseDecl()) {
      fprintf(stderr, "Parsed a top-level variable or function declaration\n");
      // if (auto *DeclIR = decl->codegen()) {
      //   DeclIR->print(errs());
      //   fprintf(stderr, "\n");
      // }
    }
    ParseDeclListPrime();
  } else if (CurTok.mType == EOF_TOK) { // FOLLOW(decl_list_prime)
    // expand by decl_list_prime ::= ε
    // do nothing
  } else { // syntax error
    LogError(CurTok, "expected 'void', 'int', 'bool' or 'float' or EOF token");
  }
}

// decl_list ::= decl decl_list_prime
void Parser::ParseDeclList() {
  auto decl = ParseDecl();
  if (decl) {
    fprintf(stderr, "Parsed a top-level variable or function declaration\n");
    // if (auto *DeclIR = decl->codegen()) {
    //   DeclIR->print(errs());
    //   fprintf(stderr, "\n");
    // }
    ParseDeclListPrime();
  }
}

// extern ::= "extern" type_spec IDENT "(" params ")" ";"
std::unique_ptr<FunctionPrototypeAST> Parser::ParseExtern() {
  std::string IdName;
  TOKEN PrevTok;

  if (CurTok.mType == EXTERN) {
    getNextToken(); // eat the EXTERN

    if (CurTok.mType == VOID_TOK || CurTok.mType == INT_TOK ||
        CurTok.mType == FLOAT_TOK || CurTok.mType == BOOL_TOK) {

      PrevTok = CurTok; // to keep track of the type token
      getNextToken();   // eat the VOID_TOK, INT_TOK, BOOL_TOK or FLOAT_TOK

      if (CurTok.mType == IDENT) {
        IdName = CurTok.getIdentifierStr(); // save the identifier name
        auto ident = std::make_unique<VariableASTnode>(CurTok, IdName);
        getNextToken(); // eat the IDENT

        if (CurTok.mType ==
            LPAR) {       // found '(' - this is an extern function declaration.
          getNextToken(); // eat (

          auto P =
              ParseParams(); // parse the parameters, returns a vector of params
          if (P.size() == 0)
            return nullptr;
          else
            fprintf(stderr, "Parsed parameter list for external function\n");

          if (CurTok.mType != RPAR) // syntax error
            return LogErrorP(
                CurTok, "expected ')' in closing extern function declaration");

          getNextToken(); // eat )

          if (CurTok.mType == SC) {
            getNextToken(); // eat ";"
            auto Proto = std::make_unique<FunctionPrototypeAST>(
                IdName, PrevTok.mLexeme, std::move(P));
            return Proto;
          } else
            return LogErrorP(
                CurTok,
                "expected ;' in ending extern function declaration statement");
        } else
          return LogErrorP(CurTok,
                           "expected (' in extern function declaration");
      }

    } else
      LogErrorP(CurTok, "expected 'void', 'int' or 'float' in extern function "
                        "declaration\n"); // syntax error
  }

  return nullptr;
}

// extern_list_prime ::= extern extern_list_prime
//                   |  ε
void Parser::ParseExternListPrime() {

  if (CurTok.mType == EXTERN) { // FIRST(extern)
    if (auto Extern = ParseExtern()) {
      fprintf(stderr,
              "Parsed a top-level external function declaration -- 2\n");

      // if (auto *ExternIR = Extern->codegen()) {
      //   ExternIR->print(errs());
      //   fprintf(stderr, "\n");
      // }
    }
    ParseExternListPrime();
  } else if (CurTok.mType == VOID_TOK || CurTok.mType == INT_TOK ||
             CurTok.mType == FLOAT_TOK ||
             CurTok.mType == BOOL_TOK) { // FOLLOW(extern_list_prime)
    // expand by decl_list_prime ::= ε
    // do nothing
  } else { // syntax error
    LogError(CurTok, "expected 'extern' or 'void',  'int' ,  'float',  'bool'");
  }
}

// extern_list ::= extern extern_list_prime
void Parser::ParseExternList() {
  auto Extern = ParseExtern();
  if (Extern) {
    fprintf(stderr, "Parsed a top-level external function declaration -- 1\n");

    // TODO: remove the code generation code from here 
    // if (auto *ExternIR = Extern->codegen()) {
    //   ExternIR->print(errs());
    //   fprintf(stderr, "\n");
    // }
    // fprintf(stderr, "Current token: %s \n", CurTok.lexeme.c_str());
    if (CurTok.mType == EXTERN)
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