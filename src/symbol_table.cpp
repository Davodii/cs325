#include "../include/symbol_table.h"
#include <stdexcept>

SymbolTable::SymbolTable() {
    // Start with a global scope
    scopeStack.emplace_back();
}

void SymbolTable::enterScope() {
    // Enter a new scope by pushing a new map onto the stack
    scopeStack.emplace_back();
}

void SymbolTable::leaveScope() {
    // Leave the current scope by popping the top map off the stack
    if (scopeStack.size() > 1) {
        scopeStack.pop_back();
    } else {
        // Error: Attempting to leave global scope
        // TODO: create and throw a specific semantic error exception
        throw std::runtime_error("Cannot leave global scope");
    }
}

bool SymbolTable::addSymbol(Symbol* symbol) {
    auto &currentScope = scopeStack.back();
    // Check for re-declaration in the current scope
    if (currentScope.find(symbol->getName()) != currentScope.end()) {
        return false; // Re-declaration found
    }

    // Note: Only checking the current scope for re-declaration.
    //       This allows for shadowing of variables in inner scopes.

    // Add the symbol to the current scope
    currentScope[symbol->getName()] = symbol;
    return true;
}

Symbol *SymbolTable::lookup(const std::string &name) {
    // Search from the innermost scope to the outermost
    for (auto scopeIt = scopeStack.rbegin(); scopeIt != scopeStack.rend();
         ++scopeIt) {
        auto it = scopeIt->find(name);
        if (it != scopeIt->end()) {
            return it->second; // Found the symbol
        }
    }
    return nullptr; // Symbol not found
}

bool SymbolTable::isDefinedInCurrentScope(const std::string &name) const {
    const auto &currentScope = scopeStack.back();
    return currentScope.find(name) != currentScope.end();
}