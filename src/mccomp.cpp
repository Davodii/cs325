#include "ast.h"
#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "error_reporter.h"

#include <cstdio>
#include <iostream>
#include <memory>

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cout << "Usage: ./code InputFile\n";
        return 1;
    }

    // Initialise the error reporter
    ErrorReporter errorReporter;

    // Initialize Lexer
    Lexer lexer(argv[1]);

    // Create the Parser
    Parser parser(lexer, errorReporter);

    // Run parser to build the AST
    std::unique_ptr<ProgramAST> ast;
    try {
        ast = parser.parse();
    } catch (const std::exception &e) {
        fprintf(stderr, "%s\n", e.what());
        return 2;
    }
    fprintf(stderr, "Parsing finished!\n");

    // NOTE: For debugging, print the AST
    fprintf(stderr, "%s\n", ast->to_string().c_str());

    // Initialize code generation
    TheModule = std::make_unique<Module>("mini-c", TheContext);

    // Generate code from AST
    // for (const auto& node : ast) {
    //   if (node) {
    //     node->codegen();
    //   }
    // }

    // TODO: Use CodegenVisitor to generate code

    // Print IR
    printf("********************* FINAL IR (begin) "
           "****************************\n");
    // Print out all of the generated code into a file called output.ll
    // printf("%s\n", argv[1]);
    auto Filename = "output.ll";
    std::error_code EC;
    raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

    if (EC) {
        errs() << "Could not open file: " << EC.message();
        return 1;
    }
    // TheModule->print(errs(), nullptr); // print IR to terminal
    // TheModule->print(dest, nullptr);
    printf("********************* FINAL IR (end) "
           "******************************\n");

    // fclose(pFile); // close the file that contains the code that was parsed
    return 0;
}
