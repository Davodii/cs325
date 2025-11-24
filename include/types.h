#ifndef MC_TYPES_H
#define MC_TYPES_H

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