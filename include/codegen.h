#ifndef MC_CODEGEN_H
#define MC_CODEGEN_H

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <llvm/IR/Value.h>
#include <memory>
#include <stack>
#include <string>
#include <vector>

#include "ast_visitor.h"
#include "error_reporter.h"
#include "symbol.h"
#include "symbol_table.h"

#include "ast.h"

using namespace llvm;
using namespace llvm::sys;

class CodegenSymbolTable {
    public:
    CodegenSymbolTable(llvm::Module &module) : mModule(module) {
        enterScope();
    }

    void enterScope() {
        mScopes.emplace_back();
    }

    void exitScope() {
        if (!mScopes.empty()) {
            mScopes.pop_back();
        }
    }

    void insert(const std::string &name, llvm::AllocaInst *alloca) {
        if (!mScopes.empty()) {
            mScopes.back()[name] = alloca;
        }
    }

    llvm::AllocaInst *lookup(const std::string &name) {
        // Search from innermost to outermost scope
        for (auto scopeIt = mScopes.rbegin(); scopeIt != mScopes.rend(); ++scopeIt) {
            auto &scope = *scopeIt;
            auto it = scope.find(name);
            if (it != scope.end()) {
                return it->second; // Found in local scope
            }
        }

        // Check for global variable
        llvm::GlobalVariable *globalVar = mModule.getGlobalVariable(name);
        if (globalVar) {
            // TODO: Handle global variable types properly
            return llvm::cast<llvm::AllocaInst>(globalVar);
        }

        return nullptr; // Not found
    }

    std::map<std::string, llvm::AllocaInst*> &currentScope() {
        return mScopes.back();
    }

    private:
    std::vector<std::map<std::string, llvm::AllocaInst*>> mScopes;
    llvm::Module &mModule; // Needed for global lookups
};

class CodeGeneration : public ASTVisitor {
    public:
        CodeGeneration(ErrorReporter &errorReporter)
            : mErrorReporter(errorReporter),
              mLlvmBuilder(mLlvmContext),
              mLlvmModule("mini-c", mLlvmContext)
        {
            mSymbolTable = std::make_unique<CodegenSymbolTable>(mLlvmModule);
        }

    /// Generate LLVM IR from the AST
    void generateCode(std::unique_ptr<ProgramAST> program);

    void visit(ProgramAST &) override;

    llvm::Type *convertTYPEToLLVMType(TYPE type) {
        switch (type) {
            case TYPE::INT:
                return llvm::Type::getInt32Ty(mLlvmContext);
            case TYPE::FLOAT:
                return llvm::Type::getFloatTy(mLlvmContext);
            case TYPE::BOOL:
                return llvm::Type::getInt1Ty(mLlvmContext);
            case TYPE::VOID:
                return llvm::Type::getVoidTy(mLlvmContext);
            default:
                return nullptr; // Handle unsupported types
        }
    }

    // --- Implementations for literal nodes ---
    void visit(IntToFloatCastAST &) override;
    void visit(IntASTnode &) override;
    void visit(BoolASTnode &) override;
    void visit(FloatASTnode &) override;
    void visit(VariableASTnode &) override;
    // --- Implementations for expressions ---
    void visit(AssignExprAST &) override;
    void visit(BinaryExprAST &) override;
    void visit(UnaryExprAST &) override;
    void visit(ArgsAST &) override;
    void visit(CallExprAST &) override;
    // --- Implementations for declarations ---
    void visit(ParamAST &) override;
    void visit(VarDeclAST &) override;
    void visit(GlobVarDeclAST &) override;
    void visit(BlockAST &) override;
    void visit(FunctionPrototypeAST &) override;
    void visit(FunctionDeclAST &) override;
    // --- Implementations for statements ---
    void visit(IfExprAST &) override;
    void visit(WhileExprAST &) override;
    void visit(ReturnAST &) override;
private:
    ErrorReporter &mErrorReporter;

    llvm::Value *mLastValue = nullptr; // Store the last generated LLVM value
    bool mIsLValue = false; // Track if the current expression is an l-value

    // Symbol table stack for all variables in scope
    std::unique_ptr<CodegenSymbolTable> mSymbolTable;

    llvm::LLVMContext mLlvmContext;
    llvm::IRBuilder<> mLlvmBuilder;
    llvm::Module mLlvmModule;

};

#endif