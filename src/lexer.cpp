#include "lexer.h"
#include "parser.h"
#include "source_location.h"
#include <cstdio>

Lexer::Lexer(const char *filename) {
    pFile = fopen(filename, "r");
    if (pFile == NULL) {
        // TODO: use fprintf() like the rest of the other function
        perror("Error opening file.");
        exit(1);
    }

    mLineNo = 1;
    mColumnNo = 1;
}

Lexer::~Lexer() {
    if (pFile) {
        fclose(pFile);
    }
}

// TODO: Create a lexer exception class to throw instead of using exit()

const std::string TOKEN::getIdentifierStr() const {
    if (type != TOKEN_TYPE::IDENT) {
        fprintf(stderr, "%d:%d Error: %s\n", loc.line, loc.column,
                "getIdentifierStr called on non-IDENT token");
        exit(2);
    }
    return lexeme;
}

const int TOKEN::getIntVal() const {
    if (type != TOKEN_TYPE::INT_LIT) {
        fprintf(stderr, "%d:%d Error: %s\n", loc.line, loc.column,
                "getIntVal called on non-INT_LIT token");
        exit(2);
    }
    return strtod(lexeme.c_str(), nullptr);
}

const float TOKEN::getFloatVal() const {
    if (type != TOKEN_TYPE::FLOAT_LIT) {
        fprintf(stderr, "%d:%d Error: %s\n", loc.line, loc.column,
                "getFloatVal called on non-FLOAT_LIT token");
        exit(2);
    }
    return strtof(lexeme.c_str(), nullptr);
}

const bool TOKEN::getBoolVal() const {
    if (type != TOKEN_TYPE::BOOL_LIT) {
        fprintf(stderr, "%d:%d Error: %s\n", loc.line, loc.column,
                "getBoolVal called on non-BOOL_LIT token");
        exit(2);
    }
    return (lexeme == "true");
}

TOKEN Lexer::returnToken(std::string lexVal, TOKEN_TYPE tokType) {
    TOKEN return_tok;
    return_tok.lexeme = lexVal;
    return_tok.type = tokType;
    return_tok.loc = SourceLoc {
        mLineNo,
        mColumnNo - static_cast<int>(lexVal.length()) - 1,
    };
    return return_tok;
}

TOKEN Lexer::peekToken() {
    if (!mHasPeekedToken) {
        mPeekedToken = getNextToken();
        mHasPeekedToken = true;
    }
    return mPeekedToken;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
TOKEN Lexer::getNextToken() {

    if (mHasPeekedToken) {
        mHasPeekedToken = false;
        return mPeekedToken;
    }

    static int LastChar = ' ';
    static int NextChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar)) {
        if (LastChar == '\n' || LastChar == '\r') {
            mLineNo++;
            mColumnNo = 1;
        }
        LastChar = getc(pFile);
        mColumnNo++;
    }

    if (isalpha(LastChar) ||
        (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
        mGlobalLexeme = LastChar;
        mColumnNo++;

        while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
            mGlobalLexeme += LastChar;
            mColumnNo++;
        }

        if (mGlobalLexeme == "int")
            return returnToken("int", TOKEN_TYPE::INT_TOK);
        if (mGlobalLexeme == "bool")
            return returnToken("bool", TOKEN_TYPE::BOOL_TOK);
        if (mGlobalLexeme == "float")
            return returnToken("float", TOKEN_TYPE::FLOAT_TOK);
        if (mGlobalLexeme == "void")
            return returnToken("void", TOKEN_TYPE::VOID_TOK);
        if (mGlobalLexeme == "extern")
            return returnToken("extern", TOKEN_TYPE::EXTERN);
        if (mGlobalLexeme == "if")
            return returnToken("if", TOKEN_TYPE::IF);
        if (mGlobalLexeme == "else")
            return returnToken("else", TOKEN_TYPE::ELSE);
        if (mGlobalLexeme == "while")
            return returnToken("while", TOKEN_TYPE::WHILE);
        if (mGlobalLexeme == "return")
            return returnToken("return", TOKEN_TYPE::RETURN);
        if (mGlobalLexeme == "true") {
            //   BoolVal = true;
            return returnToken("true", TOKEN_TYPE::BOOL_LIT);
        }
        if (mGlobalLexeme == "false") {
            //   BoolVal = false;
            return returnToken("false", TOKEN_TYPE::BOOL_LIT);
        }
        return returnToken(mGlobalLexeme.c_str(), TOKEN_TYPE::IDENT);
    }

    if (LastChar == '=') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // EQ: ==
            LastChar = getc(pFile);
            mColumnNo += 2;
            return returnToken("==", TOKEN_TYPE::EQ);
        } else {
            LastChar = NextChar;
            mColumnNo++;
            return returnToken("=", TOKEN_TYPE::ASSIGN);
        }
    }

    if (LastChar == '{') {
        LastChar = getc(pFile);
        mColumnNo++;
        return returnToken("{", TOKEN_TYPE::LBRA);
    }
    if (LastChar == '}') {
        LastChar = getc(pFile);
        mColumnNo++;
        return returnToken("}", TOKEN_TYPE::RBRA);
    }
    if (LastChar == '(') {
        LastChar = getc(pFile);
        mColumnNo++;
        return returnToken("(", TOKEN_TYPE::LPAR);
    }
    if (LastChar == ')') {
        LastChar = getc(pFile);
        mColumnNo++;
        return returnToken(")", TOKEN_TYPE::RPAR);
    }
    if (LastChar == ';') {
        LastChar = getc(pFile);
        mColumnNo++;
        return returnToken(";", TOKEN_TYPE::SC);
    }
    if (LastChar == ',') {
        LastChar = getc(pFile);
        mColumnNo++;
        return returnToken(",", TOKEN_TYPE::COMMA);
    }

    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
        std::string NumStr;

        if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
            do {
                NumStr += LastChar;
                LastChar = getc(pFile);
                mColumnNo++;
            } while (isdigit(LastChar));

            //   FloatVal = strtof(NumStr.c_str(), nullptr);
            return returnToken(NumStr, TOKEN_TYPE::FLOAT_LIT);
        } else {
            do { // Start of Number: [0-9]+
                NumStr += LastChar;
                LastChar = getc(pFile);
                mColumnNo++;
            } while (isdigit(LastChar));

            if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
                do {
                    NumStr += LastChar;
                    LastChar = getc(pFile);
                    mColumnNo++;
                } while (isdigit(LastChar));

                // FloatVal = strtof(NumStr.c_str(), nullptr);
                return returnToken(NumStr, TOKEN_TYPE::FLOAT_LIT);
            } else { // Integer : [0-9]+
                // IntVal = strtod(NumStr.c_str(), nullptr);
                return returnToken(NumStr, TOKEN_TYPE::INT_LIT);
            }
        }
    }

    if (LastChar == '&') {
        NextChar = getc(pFile);
        if (NextChar == '&') { // AND: &&
            LastChar = getc(pFile);
            mColumnNo += 2;
            return returnToken("&&", TOKEN_TYPE::AND);
        } else {
            LastChar = NextChar;
            mColumnNo++;
            return returnToken("&", static_cast<TOKEN_TYPE>(int('&')));
        }
    }

    if (LastChar == '|') {
        NextChar = getc(pFile);
        if (NextChar == '|') { // OR: ||
            LastChar = getc(pFile);
            mColumnNo += 2;
            return returnToken("||", TOKEN_TYPE::OR);
        } else {
            LastChar = NextChar;
            mColumnNo++;
            return returnToken("|", static_cast<TOKEN_TYPE>(int('|')));
        }
    }

    if (LastChar == '!') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // NE: !=
            LastChar = getc(pFile);
            mColumnNo += 2;
            return returnToken("!=", TOKEN_TYPE::NE);
        } else {
            LastChar = NextChar;
            mColumnNo++;
            return returnToken("!", TOKEN_TYPE::NOT);
            ;
        }
    }

    if (LastChar == '<') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // LE: <=
            LastChar = getc(pFile);
            mColumnNo += 2;
            return returnToken("<=", TOKEN_TYPE::LE);
        } else {
            LastChar = NextChar;
            mColumnNo++;
            return returnToken("<", TOKEN_TYPE::LT);
        }
    }

    if (LastChar == '>') {
        NextChar = getc(pFile);
        if (NextChar == '=') { // GE: >=
            LastChar = getc(pFile);
            mColumnNo += 2;
            return returnToken(">=", TOKEN_TYPE::GE);
        } else {
            LastChar = NextChar;
            mColumnNo++;
            return returnToken(">", TOKEN_TYPE::GT);
        }
    }

    if (LastChar ==
        '/') { // could be division or could be the start of a comment
        LastChar = getc(pFile);
        mColumnNo++;
        if (LastChar == '/') { // definitely a comment
            do {
                LastChar = getc(pFile);
                mColumnNo++;
            } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

            if (LastChar != EOF)
                return getNextToken();
        } else
            return returnToken("/", TOKEN_TYPE::DIV);
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF) {
        mColumnNo++;
        return returnToken("0", TOKEN_TYPE::EOF_TOK);
    }

    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    std::string s(1, ThisChar);
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken(s, static_cast<TOKEN_TYPE>(int(ThisChar)));
}
