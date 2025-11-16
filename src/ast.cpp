#include "ast.h"
#include "codegen.h"

// TODO: Provide definitions for the methods that are declared in ast.h but not defined.

TYPE VariableASTnode::getType() {
    // This should ideally look up the variable in the symbol table.
    // For now, returning a default to allow linking.

    // TODO: Implement symbol table lookup to get the actual type.
    return TYPE::INT;
}

TYPE BinaryExprAST::getType() {
    // This should depend on the operation and operand types.
    // Returning a default to allow linking.

    // TODO: Implement proper type checking and promotion rules.
    return mLeft->getType();
}