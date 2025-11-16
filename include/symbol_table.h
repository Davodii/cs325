#ifndef MC_SYMBOL_TABLE_H
#define MC_SYMBOL_TABLE_H

#include "ast.h"
#include <string>
#include <vector>
#include <map>
#include <memory>

struct Symbol {
    std::string name;
    TYPE mType;

    // TODO: add pointer to declaration node?
};

class SymbolTable {
    std::vector<std::map<std::string, Symbol>> scopeStack;
public:
    SymbolTable();

    /// Enter a new scope
    void enterScope();

    /// Leave the current scope
    void leaveScope();

    /// Add a new symbol to the current scope. Returns false on re-declaration.
    /// TODO: this should probably be throwing an error since this is not allowed
    bool addSymbol(const Symbol &symbol);

    /// Look up a symbol, searching from inner-most scope outwards.
    Symbol* lookup(const std::string &name);
};

#endif