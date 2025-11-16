#ifndef MC_SYMBOL_H
#define MC_SYMBOL_H

#include <string>
#include "types.h"


struct Symbol {
    std::string name;
    TYPE type;
    IDENT_TYPE identType;

    // TODO: should we be storing the llvm value here?
    // llvm::Value* value; // LLVM IR value associated with the symbol
};

#endif