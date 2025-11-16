#include "ast.h"
#include "codegen.h"

// TODO: Provide definitions for the methods that are declared in ast.h but not
// defined.

TYPE BinaryExprAST::getType() {
    // This should depend on the operation and operand types.
    // Returning a default to allow linking.

    // TODO: Implement proper type checking and promotion rules.
    return mLeft->getType();
}