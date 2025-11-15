#include "lexer.h"

Lexer::Lexer(const char* filename) {
  pFile = fopen(filename, "r");
  if (pFile == NULL) {
    // TODO: use fprintf() like the rest of the other function
    perror("Error opening file.");
    exit(1);
  }
}

Lexer::~Lexer() {
  if (pFile) {
    fclose(pFile);
  }
}

const std::string TOKEN::getIdentifierStr() const {
  if (mType != IDENT) {
    fprintf(stderr, "%d:%d Error: %s\n", mLineNo, mColumnNo,
            "getIdentifierStr called on non-IDENT token");
    exit(2);
  }
  return mLexeme;
}

const int TOKEN::getIntVal() const {
  if (mType != INT_LIT) {
    fprintf(stderr, "%d:%d Error: %s\n", mLineNo, mColumnNo,
            "getIntVal called on non-INT_LIT token");
    exit(2);
  }
  return strtod(mLexeme.c_str(), nullptr);
}

const float TOKEN::getFloatVal() const {
  if (mType != FLOAT_LIT) {
    fprintf(stderr, "%d:%d Error: %s\n", mLineNo, mColumnNo,
            "getFloatVal called on non-FLOAT_LIT token");
    exit(2);
  }
  return strtof(mLexeme.c_str(), nullptr);
}

const bool TOKEN::getBoolVal() const {
  if (mType != BOOL_LIT) {
    fprintf(stderr, "%d:%d Error: %s\n", mLineNo, mColumnNo,
            "getBoolVal called on non-BOOL_LIT token");
    exit(2);
  }
  return (mLexeme == "true");
}

TOKEN Lexer::returnToken(std::string lexVal, int tokType) {
  TOKEN return_tok;
  return_tok.mLexeme = lexVal;
  return_tok.mType = tokType;
  return_tok.mLineNo = mLineNo;
  return_tok.mColumnNo = mColumnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
TOKEN Lexer::getNextToken() {

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
      return returnToken("int", INT_TOK);
    if (mGlobalLexeme == "bool")
      return returnToken("bool", BOOL_TOK);
    if (mGlobalLexeme == "float")
      return returnToken("float", FLOAT_TOK);
    if (mGlobalLexeme == "void")
      return returnToken("void", VOID_TOK);
    if (mGlobalLexeme == "bool")
      return returnToken("bool", BOOL_TOK);
    if (mGlobalLexeme == "extern")
      return returnToken("extern", EXTERN);
    if (mGlobalLexeme == "if")
      return returnToken("if", IF);
    if (mGlobalLexeme == "else")
      return returnToken("else", ELSE);
    if (mGlobalLexeme == "while")
      return returnToken("while", WHILE);
    if (mGlobalLexeme == "return")
      return returnToken("return", RETURN);
    if (mGlobalLexeme == "true") {
      //   BoolVal = true;
      return returnToken("true", BOOL_LIT);
    }
    if (mGlobalLexeme == "false") {
      //   BoolVal = false;
      return returnToken("false", BOOL_LIT);
    }
    return returnToken(mGlobalLexeme.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      mColumnNo += 2;
      return returnToken("==", EQ);
    } else {
      LastChar = NextChar;
      mColumnNo++;
      return returnToken("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    mColumnNo++;
    return returnToken(",", COMMA);
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
      return returnToken(NumStr, FLOAT_LIT);
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
        return returnToken(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        // IntVal = strtod(NumStr.c_str(), nullptr);
        return returnToken(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      mColumnNo += 2;
      return returnToken("&&", AND);
    } else {
      LastChar = NextChar;
      mColumnNo++;
      return returnToken("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      mColumnNo += 2;
      return returnToken("||", OR);
    } else {
      LastChar = NextChar;
      mColumnNo++;
      return returnToken("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      mColumnNo += 2;
      return returnToken("!=", NE);
    } else {
      LastChar = NextChar;
      mColumnNo++;
      return returnToken("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      mColumnNo += 2;
      return returnToken("<=", LE);
    } else {
      LastChar = NextChar;
      mColumnNo++;
      return returnToken("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      mColumnNo += 2;
      return returnToken(">=", GE);
    } else {
      LastChar = NextChar;
      mColumnNo++;
      return returnToken(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
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
      return returnToken("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    mColumnNo++;
    return returnToken("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  mColumnNo++;
  return returnToken(s, int(ThisChar));
}
