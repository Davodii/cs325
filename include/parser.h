#ifndef MC_PARSER_H
#define MC_PARSER_H

#include "ast.h"
#include "error_reporter.h"
#include "lexer.h"

class Parser {
  public:
    Parser(Lexer &lexer, ErrorReporter &errorReporter)
        : mLexer(lexer), mErrorReporter(errorReporter) {
        // Prime the pump by getting the first token
        consumeToken();
    }

    /**
     * @brief Parse the entire input and return the AST.
     *
     * @return std::unique_ptr<ProgramAST>
     */
    std::unique_ptr<ProgramAST> parse();

    virtual ~Parser() = default;

  private:
    Lexer &mLexer;
    ErrorReporter &mErrorReporter;

    /**
     * @brief The current token being processed.
     *
     */
    TOKEN mCurrentToken;

    /**
     * @brief Convert a string to its equivalent TYPE enum.
     *
     * @param s
     * @return TYPE
     */
    TYPE stringToType(const std::string &s) const;

    /**
     * @brief Consume the next token from the lexer or from the token buffer.
     *
     */
    void consumeToken();

    /**
     * @brief Put back a token into the token buffer.
     *
     * @param token
     */
    // void putBackToken(TOKEN token);

    // --- Literals ---

    /**
     * @brief Parse a float number expression.
     *
     * float_number_expr ::= FLOAT_LIT
     *
     * @return std::unique_ptr<FloatASTnode>
     */
    std::unique_ptr<FloatASTnode> ParseFloatNumberExpr();

    /**
     * @brief Parse an integer number expression.
     *
     * int_number_expr ::= INT_LIT
     *
     * @return std::unique_ptr<IntASTnode>
     */
    std::unique_ptr<IntASTnode> ParseIntNumberExpr();

    /**
     * @brief Parse a boolean expression.
     *
     * bool_expr ::= BOOL_LIT
     *
     * @return std::unique_ptr<BoolASTnode>
     */
    std::unique_ptr<BoolASTnode> ParseBoolExpr();

    // --- Expressions ---
    /**
     * @brief Parse a primary expression.
     *
     * primary ::= IDENT primary_tail
     *         | INT_LIT
     *         | FLOAT_LIT
     *         | BOOL_LIT
     *         | "(" expr ")"
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParsePrimary();

    /**
     * @brief Parse the tail of a primary expression (function call).
     *
     * primary_tail ::= "(" args ")"
     *                | ε
     *
     * @param Callee The identifier of the function being called.
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST>
    ParsePrimaryTail(std::unique_ptr<VariableASTnode> Callee);

    /**
     * @brief Parse arguments for a function call.
     *
     * args ::= expr args_tail
     *        | ε
     *
     * @return std::vector<std::unique_ptr<ExprAST>>
     */
    std::vector<std::unique_ptr<ExprAST>> ParseArgs();

    /**
     * @brief Parse the tail of arguments for a function call.
     *
     * args_tail ::= "," expr args_tail
     *             | ε
     *
     * @return std::vector<std::unique_ptr<ExprAST>>
     */
    std::vector<std::unique_ptr<ExprAST>> ParseArgsTail();

    /**
     * @brief Parse a unary expression.
     *
     * unary ::= ("-" | "!") unary
     *         | primary
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseUnary();

    /**
     * @brief Parse a multiplicative expression.
     *
     * mul ::= unary mul_prime
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseMultiplicative();

    /**
     * @brief Parse an additive expression.
     *
     * additive ::= multiplicative additive_prime
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseAdditive();

    /**
     * @brief Parse a relational expression.
     *
     * relational ::= additive relational_prime
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseRelational();

    /**
     * @brief Parse an equality expression.
     *
     * equality ::= relational equality_prime
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseEquality();

    /**
     * @brief Parse a logical AND expression.
     *
     * logical_and ::= equality logical_and_prime
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseLogicalAnd();

    /**
     * @brief Parse a logical OR expression.
     *
     * logical_or ::= logical_and logical_or_prime
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseLogicalOr();

    /**
     * @brief Parse an expression.
     *
     * expr ::= assignment 
     *        | logical_or
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseExper();

    // --- Declarations ---
    /**
     * @brief Parse the parameter list.
     *
     * param_list ::= param param_list_prime
     *            |  ε
     *
     * @return std::vector<std::unique_ptr<ParamAST>>
     */
    std::vector<std::unique_ptr<ParamAST>> ParseParamList();

    /**
     * @brief Parse a single parameter.
     *
     * param ::= var_type IDENT
     *
     * @return std::unique_ptr<ParamAST>
     */
    std::unique_ptr<ParamAST> ParseParam();

    /**
     * @brief Parse a list of declarations.
     *
     * decl_list ::= decl decl_list_prime
     *           |  ε
     *
     * @return std::vector<std::unique_ptr<DeclAST>>
     */
    std::vector<std::unique_ptr<DeclAST>> ParseDeclList();

    /**
     * @brief Parse a single declaration.
     *
     * decl ::= var_type IDENT ";"
     *      |  var_type IDENT "(" params ")" block
     *
     * @return std::unique_ptr<DeclAST>
     */
    std::unique_ptr<DeclAST> ParseDecl();

    /**
     * @brief Parse a local variable declaration.
     *
     * local_decl ::= var_type IDENT ";"
     *
     * @return std::unique_ptr<VarDeclAST>
     */
    std::unique_ptr<VarDeclAST> ParseLocalDecl();

    /**
     * @brief Parse a list of local variable declarations.
     *
     * local_decls ::= local_decl local_decls_prime
     *             |  ε
     *
     * @return std::vector<std::unique_ptr<VarDeclAST>>
     */
    std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDeclList();
    std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDeclListPrime();

    // --- Statements ---
    /**
     * @brief Parse a list of statements.
     *
     * stmt_list ::= stmt stmt_list_prime
     *           |  ε
     *
     * @return std::vector<std::unique_ptr<ASTnode>>
     */
    std::vector<std::unique_ptr<ASTnode>> ParseStmtList();
    std::vector<std::unique_ptr<ASTnode>> ParseStmtListPrime();

    /**
     * @brief Parse a single statement.
     *
     * stmt ::= expr_stmt
     *      |  block
     *      |  if_stmt
     *      |  while_stmt
     *      |  return_stmt
     *
     * @return std::unique_ptr<ASTnode>
     */
    std::unique_ptr<ASTnode> ParseStmt();

    /**
     * @brief Parse an expression statement.
     *
     * expr_stmt ::= expr ";"
     *           |  ";"
     *
     * @return std::unique_ptr<ExprAST>
     */
    std::unique_ptr<ExprAST> ParseExperStmt();

    /**
     * @brief Parse a block of statements.
     *
     * block ::= "{" local_decls stmt_list "}"
     *
     * @return std::unique_ptr<BlockAST>
     */
    std::unique_ptr<BlockAST> ParseBlock(bool isFunctionBody = false);

    /**
     * @brief Parse an if statement.
     *
     * if_stmt ::= "if" "(" expr ")" block else_stmt
     *
     * @return std::unique_ptr<ASTnode>
     */
    std::unique_ptr<IfExprAST> ParseIfStmt();

    /**
     * @brief Parse an else statement.
     *
     * else_stmt  ::= "else" block
     *            |  ε
     *
     * @return std::unique_ptr<BlockAST>
     */
    std::unique_ptr<BlockAST> ParseElseStmt();

    /**
     * @brief Parse a while statement.
     *
     * while_stmt ::= "while" "(" expr ")" stmt
     *
     * @return std::unique_ptr<WhileExprAST>
     */
    std::unique_ptr<WhileExprAST> ParseWhileStmt();

    /**
     * @brief Parse a return statement.
     *
     * return_stmt ::= "return" expr ";"
     *
     * @return std::unique_ptr<ReturnAST>
     */
    std::unique_ptr<ReturnAST> ParseReturnStmt();

    // --- Extern Functions ---
    /**
     * @brief Parse a list of extern function prototypes.
     *
     * extern_list ::= extern extern_list_prime
     *             |  ε
     *
     * @return std::vector<std::unique_ptr<FunctionPrototypeAST>>
     */
    std::vector<std::unique_ptr<FunctionPrototypeAST>> ParseExternList();

    /**
     * @brief Parse a single extern function prototype.
     *
     * extern ::= "extern" var_type IDENT "(" params ")" ";"
     *
     * @return std::unique_ptr<FunctionPrototypeAST>
     */
    std::unique_ptr<FunctionPrototypeAST> ParseExtern();
};

#endif