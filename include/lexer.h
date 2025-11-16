#ifndef MC_LEXER_H
#define MC_LEXER_H

#include <string>

// The lexer returns one of these for known things.
enum class TOKEN_TYPE {

    IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
    ASSIGN = int('='), // '='

    // delimiters
    LBRA = int('{'),  // left brace
    RBRA = int('}'),  // right brace
    LPAR = int('('),  // left parenthesis
    LBOX = int('['),  // left bracket
    RBOX = int(']'),  // right bracket
    RPAR = int(')'),  // right parenthesis
    SC = int(';'),    // semicolon
    COMMA = int(','), // comma

    // types
    INT_TOK = -2,   // "int"
    VOID_TOK = -3,  // "void"
    FLOAT_TOK = -4, // "float"
    BOOL_TOK = -5,  // "bool"

    // keywords
    EXTERN = -6,  // "extern"
    IF = -7,      // "if"
    ELSE = -8,    // "else"
    WHILE = -9,   // "while"
    RETURN = -10, // "return"
    // TRUE   = -12,     // "true"
    // FALSE   = -13,     // "false"

    // literals
    INT_LIT = -14,   // [0-9]+
    FLOAT_LIT = -15, // [0-9]+.[0-9]+
    BOOL_LIT = -16,  // "true" or "false" key words

    // logical operators
    AND = -17, // "&&"
    OR = -18,  // "||"

    // operators
    PLUS = int('+'),    // addition or unary plus
    MINUS = int('-'),   // substraction or unary negative
    ASTERIX = int('*'), // multiplication
    DIV = int('/'),     // division
    MOD = int('%'),     // modular
    NOT = int('!'),     // unary negation

    // comparison operators
    EQ = -19,      // equal
    NE = -20,      // not equal
    LE = -21,      // less than or equal to
    LT = int('<'), // less than
    GE = -23,      // greater than or equal to
    GT = int('>'), // greater than

    // special tokens
    EOF_TOK = 0, // signal end of file

    // invalid
    INVALID = -100 // signal invalid token
};

// TOKEN class is used to keep track of information about a token
class TOKEN {
  public:
    TOKEN() = default;
    TOKEN_TYPE type = TOKEN_TYPE::INVALID;
    std::string lexeme;
    int lineNo;
    int columnNo;
    const std::string getIdentifierStr() const;
    const int getIntVal() const;
    const float getFloatVal() const;
    const bool getBoolVal() const;
};

class Lexer {
  public:
    Lexer(const char *filename);
    ~Lexer();

    TOKEN getNextToken();

  private:
    FILE *pFile;
    int mLineNo;
    int mColumnNo;
    std::string mGlobalLexeme;

    TOKEN returnToken(std::string lexVal, TOKEN_TYPE tokType);
};

#endif