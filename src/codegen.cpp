#include "codegen.h"
#include "ast.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <vector>

void CodeGeneration::generateCode(std::unique_ptr<ProgramAST> program) {
    // Start code generation by visiting the root node
    program->accept(*this);
}

void CodeGeneration::visit(ProgramAST &n) {}

// --- Implementations for literal nodes ---

void CodeGeneration::visit(IntToFloatCastAST &n) {
    n.getExpr()->accept(*this);
    llvm::Value *intValue = mLastValue;

    mLastValue = mLlvmBuilder.CreateSIToFP(
        intValue, 
        llvm::Type::getFloatTy(mLlvmContext), 
        "inttofloattmp"
    );

    if (!mLastValue) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate int to float cast");
    }
}

void CodeGeneration::visit(IntASTnode &n) {
    mLastValue = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(mLlvmContext), 
        n.getValue()
    );
}

void CodeGeneration::visit(BoolASTnode &n) {
    mLastValue = llvm::ConstantInt::get(
        llvm::Type::getInt1Ty(mLlvmContext), 
        n.getValue() ? 1 : 0 // true is 1, false is 0
    );
}

void CodeGeneration::visit(FloatASTnode &n) {
    mLastValue = llvm::ConstantFP::get(
        llvm::Type::getFloatTy(mLlvmContext), 
        n.getValue()
    );
}

void CodeGeneration::visit(VariableASTnode &n) {
    if (!n.getResolvedSymbol()) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Unresolved symbol for variable: " + n.getName());
        mLastValue = nullptr;
        return;
    }

    if (mIsLValue) {
        // For L-value, return the pointer to the variable
        llvm::Value *varPtr = mSymbolTable->lookup(n.getResolvedSymbol()->getName());
        
        if (!varPtr) {
            mErrorReporter.codeGenError(n.getSourceLocation(), "Undefined variable: " + n.getName());
            mLastValue = nullptr;
            return;
        }
        mLastValue = varPtr;
        return;
    }
    // For R-value, load the variable's value
    llvm::Value *varPtr = mSymbolTable->lookup(n.getResolvedSymbol()->getName());
    if (!varPtr) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Undefined variable: " + n.getName());
        mLastValue = nullptr;
        return;
    }

    // Load the variable's value
    mLastValue = mLlvmBuilder.CreateLoad(
        convertTYPEToLLVMType(n.getType()),
        varPtr,
        n.getName() + "_load"
    );

    if (!mLastValue) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate load instruction for variable: " + n.getName());
    }
}

// -- - Implementations for expressions ---

void CodeGeneration::visit(AssignExprAST &n) {
    // Save the current l-value state
    // and set it to true for the variable
    bool savedState = mIsLValue;
    mIsLValue = true;
    n.getVariable()->accept(*this);

    // Restore the previous l-value state
    // variable address is now in mLastValue
    mIsLValue = savedState;
    llvm::Value *varPtr = mLastValue;

    n.getExpression()->accept(*this);
    llvm::Value *exprValue = mLastValue;

    mLastValue = mLlvmBuilder.CreateStore(exprValue, varPtr);

    if (!mLastValue) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate store instruction");
    }
}

void CodeGeneration::visit(BinaryExprAST &n) {
    n.getLHS()->accept(*this);
    llvm::Value *left = mLastValue;

    n.getRHS()->accept(*this);
    llvm::Value *right = mLastValue;

    // Handle type-specific operations
    if (n.getType() == TYPE::FLOAT) {
        // Handle floating-point operations
        switch (n.getOperator()) {
            case TOKEN_TYPE::PLUS:
                mLastValue = mLlvmBuilder.CreateFAdd(left, right, "faddtmp");
                break;
            case TOKEN_TYPE::MINUS:
                mLastValue = mLlvmBuilder.CreateFSub(left, right, "fsubtmp");
                break;
            case TOKEN_TYPE::ASTERIX:
                mLastValue = mLlvmBuilder.CreateFMul(left, right, "fmultmp");
                break;
            case TOKEN_TYPE::DIV:
                mLastValue = mLlvmBuilder.CreateFDiv(left, right, "fdivtmp");
                break;
            default:
                // Handle unsupported operators
                mErrorReporter.codeGenError(n.getSourceLocation(), "Unsupported binary operator for float type");
                mLastValue = nullptr;
                break;
        }

        if (!mLastValue) {
            mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate floating-point binary operation");
        }
        return;
    }

    switch (n.getOperator()) {
        case TOKEN_TYPE::PLUS:
            mLastValue = mLlvmBuilder.CreateAdd(left, right, "addtmp");
            break;
        case TOKEN_TYPE::MINUS:
            mLastValue = mLlvmBuilder.CreateSub(left, right, "subtmp");
            break;
        case TOKEN_TYPE::ASTERIX:
            mLastValue = mLlvmBuilder.CreateMul(left, right, "multmp");
            break;
        case TOKEN_TYPE::DIV:
            mLastValue = mLlvmBuilder.CreateSDiv(left, right, "divtmp");
            break;
        default:
            // Handle unsupported operators
            mErrorReporter.codeGenError(n.getSourceLocation(), "Unsupported binary operator");
            mLastValue = nullptr;
            break;
    }

    if (!mLastValue) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate binary operation");
    }
}

void CodeGeneration::visit(UnaryExprAST &n) {
    n.getExpression()->accept(*this);
    llvm::Value *operand = mLastValue;

    switch (n.getOperator()) {
        case TOKEN_TYPE::MINUS:
            mLastValue = mLlvmBuilder.CreateNeg(operand, "negtmp");
            break;
        case TOKEN_TYPE::NOT:
            mLastValue = mLlvmBuilder.CreateNot(operand, "nottmp");
            break;
        default:
            // Handle unsupported operators
            mErrorReporter.codeGenError(n.getSourceLocation(), "Unsupported unary operator");
            mLastValue = nullptr;
            break;
    }

    if (!mLastValue) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate unary operation");
    }
}

void CodeGeneration::visit(ArgsAST &n) {
    // Don't need to do anything here, handled in CallExprAST
    // If we do get here, throw an error
    mErrorReporter.codeGenError(n.getSourceLocation(), "ArgsAST should not be visited directly");
}

void CodeGeneration::visit(CallExprAST &n) {
    // Get the function name
    std::string funcName = n.getCallee()->getName();

    // Lookup function in the module
    llvm::Function *calleeFunc = mLlvmModule.getFunction(funcName);
    if (!calleeFunc) {
        mErrorReporter.codeGenError(n.getSourceLocation(), "Undefined function: " + funcName);
        mLastValue = nullptr;
        return;
    }

    // Generate code for each argument
    std::vector<llvm::Value*> argValues;
    for (auto &arg : n.getArgs()->getArgsList()) {
        // Generate code for the argument
        arg->accept(*this);
        llvm::Value *argValue = mLastValue;

        if (!argValue) {
            mErrorReporter.codeGenError(n.getSourceLocation(), "Failed to generate argument for function call: " + funcName);
            mLastValue = nullptr;
            return;
        }

        argValues.push_back(argValue);
    }

    llvm::CallInst *callInst = mLlvmBuilder.CreateCall(calleeFunc, argValues, "calltmp");
    mLastValue = callInst;
}

// --- Implementations for declarations ---

void CodeGeneration::visit(ParamAST &n) {
    
}

void CodeGeneration::visit(VarDeclAST &node) {
    // 1. Get current function
    llvm::Function *TheFunction = mLlvmBuilder.GetInsertBlock()->getParent();

    // 2. Create an alloca for the variable in the entry block.
    llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                         TheFunction->getEntryBlock().begin());
    llvm::Type* llvmType = convertTYPEToLLVMType(node.getType());
    llvm::AllocaInst *Alloca = TmpB.CreateAlloca(llvmType, nullptr, node.getName());

    // 3. Store the alloca in the symbol table
    mSymbolTable->insert(node.getName(), Alloca);

    // 4. This is a statement, it does not produce a value.
    mLastValue = nullptr;
}

void CodeGeneration::visit(GlobVarDeclAST &n) {
    llvm::Type *llvmType = convertTYPEToLLVMType(n.getType());
    llvm::Constant *initialValue = llvm::Constant::getNullValue(llvmType);
    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        mLlvmModule,
        llvmType,
        false, // isConstant
        llvm::GlobalValue::ExternalLinkage,
        initialValue,
        n.getName()
    );
    // Globals don't produce a value during codegen of their declaration
    mLastValue = nullptr;
}

void CodeGeneration::visit(BlockAST &n) {
    // Enter a new scope if not a function block
    if (!n.isFunctionBlock()) {
        mSymbolTable->enterScope();
    }

    auto &currentScope = mSymbolTable->currentScope();

    for (auto &decl : n.getLocalDecls()) {
        decl->accept(*this);
    }

    for (auto &stmt : n.getStmts()) {
        stmt->accept(*this);
    }

    if (!n.isFunctionBlock()) {
        // Leave the scope
        mSymbolTable->exitScope();
    }

    // Blocks do not return a value
    mLastValue = nullptr;
}

void CodeGeneration::visit(FunctionPrototypeAST &n) {
    llvm::Type *returnType = convertTYPEToLLVMType(n.getType());

    // Convert parameter types
    std::vector<llvm::Type*> paramTypes;
    for (const auto &param : n.getParams()) {
        llvm::Type *paramType = convertTYPEToLLVMType(param->getType());
        paramTypes.push_back(paramType);
    }

    // Create function type
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        returnType,
        paramTypes,
        false // not variadic
    );

    // Create the function
    llvm::Function *function = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        n.getName(),
        mLlvmModule
    );

    // Set names for all arguments
    unsigned idx = 0;
    for (auto &arg : function->args()) {
        arg.setName(n.getParams()[idx]->getName());
        idx++;
    }

    mLastValue = function;
}

void CodeGeneration::visit(FunctionDeclAST &n) {
    // Visit the prototype to create the function
    n.getProto()->accept(*this);

    llvm::Function *function = llvm::cast<llvm::Function>(mLastValue);

    // Enter a new scope for the function
    mSymbolTable->enterScope();

    // Create a new basic block to start insertion into
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(
        mLlvmContext, "entry", function
    );
    mLlvmBuilder.SetInsertPoint(entry);

    // Create an IR builder for the entry block to handle allocas
    llvm::IRBuilder<> TmpB(&function->getEntryBlock(), function->getEntryBlock().begin());

    unsigned i = 0;
    for (auto &arg : function->args()) {
        ParamAST *paramNode = n.getProto()->getParams()[i].get();
        std::string paramName = paramNode->getName();

        llvm::Type *paramType = convertTYPEToLLVMType(paramNode->getType());

        // Create an alloca for the parameter
        llvm::AllocaInst *alloca = TmpB.CreateAlloca(paramType, nullptr, paramName);

        // Store the function argument into the alloca
        mLlvmBuilder.CreateStore(&arg, alloca);

        // Add the alloca to the symbol table
        mSymbolTable->insert(paramName, alloca);

        arg.setName(paramName);
        i++;
    }

    // Visit the function body
    n.getBlock()->accept(*this);

    // The block visitor will have set mLastValue to nullptr.
    // A function declaration also doesn't "return" a value in the context
    // of the codegen visitor, so we leave it as is.
    // We might add an implicit return for void functions here if needed.
    // For now, we assume all functions have an explicit return.

    auto *block = mLlvmBuilder.GetInsertBlock();

    // Check if the function has a return statement
    if (block->getTerminator() == nullptr) {
        if (function->getReturnType()->isVoidTy()) {
            mLlvmBuilder.CreateRetVoid();
        } else {
            // Non-void function missing return statement
            // Since we are in the function body, this is place where all 
            // control paths should return a value.
            mErrorReporter.codeGenError(n.getSourceLocation(), "Non-void function missing return statement: " + n.getProto()->getName());
        }
    }

    // Exit the scope
    mSymbolTable->exitScope();

    mLastValue = function;
}

// --- Implementations for statements ---

void CodeGeneration::visit(IfExprAST &node) {
    // 1. Codegen the condition
    node.getCondition()->accept(*this);
    llvm::Value *CondV = mLastValue;
    if (!CondV) {
        mErrorReporter.codeGenError( node.getSourceLocation(), "Condition in if-statement is invalid.");
        return;
    }

    // If the condition is a float, we compare it to 0.0.
    // In C, any non-zero value is true.
    if (CondV->getType()->isFloatTy()) {
        CondV = mLlvmBuilder.CreateFCmpONE(CondV, llvm::ConstantFP::get(mLlvmContext, llvm::APFloat(0.0f)), "ifcond");
    } else if (CondV->getType()->isIntegerTy() && CondV->getType() != llvm::Type::getInt1Ty(mLlvmContext)) {
        // If it's an integer type other than i1 (bool), compare it to 0.
        CondV = mLlvmBuilder.CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
    }
    // If it's already a bool (i1), we can use it directly.

    llvm::Function *TheFunction = mLlvmBuilder.GetInsertBlock()->getParent();

    // 2. Create blocks for the then, else, and merge cases.
    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(mLlvmContext, "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(mLlvmContext, "else");
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(mLlvmContext, "ifcont");

    // 3. Create the conditional branch.
    mLlvmBuilder.CreateCondBr(CondV, ThenBB, ElseBB);

    // 4. Emit the 'then' block.
    mLlvmBuilder.SetInsertPoint(ThenBB);
    node.getThen()->accept(*this);
    mLlvmBuilder.CreateBr(MergeBB);
    // Codegen of 'then' can change the current block, update ThenBB for the PHI.
    ThenBB = mLlvmBuilder.GetInsertBlock();

    // 5. Emit the 'else' block.
    TheFunction->insert(TheFunction->end(), ElseBB);
    mLlvmBuilder.SetInsertPoint(ElseBB);
    if (node.getElse()) {
        node.getElse()->accept(*this);
    }
    mLlvmBuilder.CreateBr(MergeBB);
    // update ElseBB for the PHI.
    ElseBB = mLlvmBuilder.GetInsertBlock();

    // 6. Emit the merge block.
    TheFunction->insert(TheFunction->end(), MergeBB);
    mLlvmBuilder.SetInsertPoint(MergeBB);


    mLastValue = nullptr; // If statement produces no value.
}

void CodeGeneration::visit(WhileExprAST &node) {
    // 1. Get the current function and create the necessary basic blocks.
    llvm::Function *TheFunction = mLlvmBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *LoopHeaderBB = llvm::BasicBlock::Create(mLlvmContext, "loop.header", TheFunction);
    llvm::BasicBlock *LoopBodyBB = llvm::BasicBlock::Create(mLlvmContext, "loop.body");
    llvm::BasicBlock *AfterLoopBB = llvm::BasicBlock::Create(mLlvmContext, "after.loop");

    // 2. Jump from the current block into the loop header.
    mLlvmBuilder.CreateBr(LoopHeaderBB);

    // 3. Start emitting code in the loop header.
    mLlvmBuilder.SetInsertPoint(LoopHeaderBB);

    // 4. Evaluate the loop condition.
    node.getCondition()->accept(*this);
    llvm::Value *CondV = mLastValue;
    if (!CondV) {
        mErrorReporter.codeGenError(node.getSourceLocation(), "Condition in while-loop is invalid.");
        return;
    }

    // Convert condition to a bool (i1) if it isn't already.
    if (CondV->getType()->isFloatTy()) {
        CondV = mLlvmBuilder.CreateFCmpONE(CondV, llvm::ConstantFP::get(mLlvmContext, llvm::APFloat(0.0f)), "loopcond");
    } else if (CondV->getType()->isIntegerTy() && CondV->getType() != llvm::Type::getInt1Ty(mLlvmContext)) {
        CondV = mLlvmBuilder.CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
    }

    // 5. Create the conditional branch.
    mLlvmBuilder.CreateCondBr(CondV, LoopBodyBB, AfterLoopBB);

    // 6. Emit the loop body.
    TheFunction->insert(TheFunction->end(), LoopBodyBB);
    mLlvmBuilder.SetInsertPoint(LoopBodyBB);
    node.getBody()->accept(*this); // Visit the body statement/block

    // 7. After the body, jump back to the header.
    mLlvmBuilder.CreateBr(LoopHeaderBB);

    // 8. Set the insertion point to the block after the loop.
    TheFunction->insert(TheFunction->end(), AfterLoopBB);
    mLlvmBuilder.SetInsertPoint(AfterLoopBB);

    // A while loop is a statement and produces no value.
    mLastValue = nullptr;
}

void CodeGeneration::visit(ReturnAST &n) {
    if (n.getExpression()) {
        n.getExpression()->accept(*this);
        llvm::Value *retValue = mLastValue;

        if (!retValue) {
            mErrorReporter.codeGenError(n.getSourceLocation(), "Return expression is invalid.");
            return;
        }

        mLlvmBuilder.CreateRet(retValue);
    } else {
        // Void return
        mLlvmBuilder.CreateRetVoid();
    }
}