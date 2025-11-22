#ifndef MC_SYMBOL_H
#define MC_SYMBOL_H

#include "types.h"
#include <string>

// Forward declaration
class DeclAST;

class Symbol {
public:
    Symbol() = default;
    Symbol(const std::string &name, TYPE type, IDENT_TYPE identType, DeclAST* declaration = nullptr)
        : name(name), type(type), identType(identType), declaration(declaration) {}

    const std::string &getName() const { return name; }
    TYPE getType() const { return type; }
    IDENT_TYPE getKind() const { return identType; }
    DeclAST* getDeclaration() const { return declaration; }


private:
    std::string name;
    TYPE type;
    IDENT_TYPE identType;
    DeclAST* declaration; // Pointer to the AST node where the symbol is declared
};

#endif