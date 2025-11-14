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


#endif