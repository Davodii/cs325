#ifndef MC_TYPES_H
#define MC_TYPES_H

// TODO: this is sloppy should move IDENT_TYPE and TYPE to a separate file
// so both ast.h and symbol_table.h can include it without circular dependencies
#include <string>
enum class TYPE {
    INT,
    FLOAT,
    BOOL,
    VOID,
};

inline std::string typeToString(TYPE type) {
    if (type == TYPE::BOOL) {
        return "BOOL";
    }
    if (type == TYPE::INT) {
        return "INT";
    }
    if (type == TYPE::FLOAT) {
        return "FLOAT";
    }
    if (type == TYPE::VOID) {
        return "VOID";
    }
    return "UNKNOWN_TYPE";
}

enum class IDENT_TYPE {
    LOCAL,     // Local variable
    PARAMETER, // Function parameter
    FUNCTION,  // Function
    GLOBAL,    // Global variable
};

#endif