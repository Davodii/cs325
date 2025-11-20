#ifndef MC_SYMBOL_H
#define MC_SYMBOL_H

#include "types.h"
#include <string>

class Symbol {
public:
    Symbol() = default;
    Symbol(const std::string &name, TYPE type, IDENT_TYPE identType)
        : name(name), type(type), identType(identType) {}

    const std::string &getName() const { return name; }
    TYPE getType() const { return type; }
    IDENT_TYPE getIdentType() const { return identType; }
private:
    std::string name;
    TYPE type;
    IDENT_TYPE identType;

    // TODO: should we be storing the llvm value here?
    // llvm::Value* value; // LLVM IR value associated with the symbol
};

#endif