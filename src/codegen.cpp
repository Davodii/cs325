#include "codegen.h"

llvm::LLVMContext TheContext;
llvm::IRBuilder<> Builder(TheContext);
std::unique_ptr<llvm::Module> TheModule;

// void CodegenVisitor::generateCode(std::vector<std::unique_ptr<ASTnode>> &ast,
                                //   SymbolTable &symbolTable) {}
