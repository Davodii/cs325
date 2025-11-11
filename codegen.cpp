#include "codegen.h"

llvm::LLVMContext TheContext;
llvm::IRBuilder<> Builder(TheContext);
std::unique_ptr<llvm::Module> TheModule;