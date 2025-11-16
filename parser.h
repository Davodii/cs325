#ifndef MC_PARSER_H
#define MC_PARSER_H

#include <queue>
#include "lexer.h"
#include "ast.h"

#include <stdexcept>
#include <string>

class ParseError : public std::exception {
public:
    ParseError(std::string msg, int line = -1, int col = -1)
        : message(std::move(msg)), lineNumber(line), columnNumber(col) {
        if (line != -1 && col != -1) {
            formattedMessage = std::to_string(lineNumber) + ":" + std::to_string(columnNumber) + " Error: " + message;
        } else {
            formattedMessage = "Error: " + message;
        }
    }

    const char* what() const noexcept override {
        return formattedMessage.c_str();
    }

    int getLineNumber() const {
        return lineNumber;
    }

    int getColumnNumber() const {
        return columnNumber;
    }

private:
    std::string message;
    int lineNumber;
    int columnNumber;
    std::string formattedMessage;
};

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
// extern TOKEN CurTok;

// TOKEN getNextToken();
// void putBackToken(TOKEN tok);

// TODO: the parser() function is all that is run from the main() function.
// Maybe we can remove the Parse...() functions froms being publicly available.
// void parser();

class Parser {
    Lexer &lexer;

    /**
     * @brief The current token being processed.
     * 
     */
    TOKEN currentToken;

    /**
     * @brief A buffer to hold tokens that have been put back.
     * 
     */
    std::deque<TOKEN> tokenBuffer;

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
    void putBackToken(TOKEN token);

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
     * @brief Parse an expression.
     * 
     * expr ::= IDENT "=" expr
     *     |  rval
     * 
     * @return std::unique_ptr<ExprAST> 
     */
    std::unique_ptr<ExprAST> ParseExper();
    
    
    /**
     * @brief Parse an expression statement.
     * 
     * expr_stmt ::= expr ";"
     *           |  ";"
     * 
     * @return std::unique_ptr<ExprAST> 
     */
    std::unique_ptr<ExprAST> ParseExperStmt();

    // --- Declarations ---
    /**
     * @brief Parse function parameters.
     * 
     * params ::= param_list
     *        |  ε
     * 
     * @return std::vector<std::unique_ptr<ParamAST>> 
     */
    std::vector<std::unique_ptr<ParamAST>> ParseParams();

    /**
     * @brief Parse the parameter list.
     * 
     * param_list ::= param param_list_prime
     *            |  ε
     * 
     * @return std::vector<std::unique_ptr<ParamAST>> 
     */
    std::vector<std::unique_ptr<ParamAST>> ParseParamList();
    std::vector<std::unique_ptr<ParamAST>> ParseParamListPrime();

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
    std::vector<std::unique_ptr<DeclAST>> ParseDeclListPrime();

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
    std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDecls();
    std::vector<std::unique_ptr<VarDeclAST>> ParseLocalDeclsPrime();

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
     * @brief Parse a block of statements.
     * 
     * block ::= "{" local_decls stmt_list "}"
     * 
     * @return std::unique_ptr<BlockAST> 
     */
    std::unique_ptr<BlockAST> ParseBlock();

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
     * while_stmt ::= "while" "(" expr ")" block
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
    std::vector<std::unique_ptr<FunctionPrototypeAST>> ParseExternListPrime();

    /**
     * @brief Parse a single extern function prototype.
     * 
     * extern ::= "extern" var_type IDENT "(" params ")" ";"
     * 
     * @return std::unique_ptr<FunctionPrototypeAST> 
     */
    std::unique_ptr<FunctionPrototypeAST> ParseExtern();


public:
    Parser(Lexer &lexer);

    /**
     * @brief Parse the entire input and return the AST.
     * 
     * @return std::vector<std::unique_ptr<ASTnode>> 
     */
    std::vector<std::unique_ptr<ASTnode>> parse();
};

#endif