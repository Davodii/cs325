#ifndef MC_TYPES_H
#define MC_TYPES_H

// TODO: this is sloppy should move IDENT_TYPE and TYPE to a separate file
// so both ast.h and symbol_table.h can include it without circular dependencies
enum class TYPE {
    INT,
    FLOAT,
    BOOL,
};

enum class IDENT_TYPE {
    LOCAL,     // Local variable
    PARAMETER, // Function parameter
    FUNCTION,  // Function
    GLOBAL,    // Global variable
};

#endif