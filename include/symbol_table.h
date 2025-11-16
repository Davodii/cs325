#ifndef MC_SYMBOL_TABLE_H
#define MC_SYMBOL_TABLE_H

#include "ast.h"
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <stdexcept>
#include "symbol.h"

class SymbolTable {
    std::vector<std::map<std::string, Symbol>> scopeStack;
public:
    SymbolTable();

    /**
     * @brief Enter a new scope.
     * 
     */
    void enterScope();

    /**
     * @brief Leave the current scope
     * 
     */
    void leaveScope();

    /**
     * @brief Add a symbol to the current scope.
     * 
     * @param symbol The symbol to add.
     * @return true if the symbol was added successfully.
     * @return false if there was a re-declaration in the current scope.
     */
    bool addSymbol(const Symbol &symbol);

    /**
     * @brief Lookup a symbol by name, searching from the innermost scope to the outermost.
     * 
     * @param name The name of the symbol to look up.
     * @return Symbol* Pointer to the symbol if found, nullptr otherwise.
     */
    Symbol* lookup(const std::string &name);
};

#endif