#ifndef ERROR_REPORTED_H
#define ERROR_REPORTED_H

#include <stdexcept>
#include <string>
#include "source_location.h"

// Base class for compiler errors
class CompilerError : public std::exception {
  public:
    SourceLoc location;
    std::string message;

    CompilerError(SourceLoc loc, std::string msg) : location(std::move(loc)), message(std::move(msg)) {}

    const char *what() const noexcept override { 
        return message.c_str(); 
    }

    virtual ~CompilerError() = default;
};

// Syntax error
class SyntaxError : public CompilerError {
  public:
    std::string expected;
    std::string found;

    SyntaxError(SourceLoc loc, std::string expected, std::string found) : 
    CompilerError(std::move(loc), "Syntax error: Expected " + expected + ", but found " + found),
    expected(std::move(expected)), found(std::move(found)) {}
};

// Semantic error
class TypeError : public CompilerError {
  public:
    std::string expected;
    std::string found;

    TypeError(SourceLoc loc, std::string t1, std::string t2) : 
    CompilerError(std::move(loc), "Type error: expected type " + t1 + ", but found type " + t2),
    expected(std::move(t1)), found(std::move(t2)) {}
};

class UndefinedVariableError : public CompilerError {
  public:
    std::string varName;

    UndefinedVariableError(SourceLoc loc, std::string name) : 
    CompilerError(std::move(loc), "Undefined variable: " + name),
    varName(std::move(name)) {}
};

class RedefinitionError : public CompilerError {
  public:
    std::string name;

    RedefinitionError(SourceLoc loc, std::string name) : 
    CompilerError(std::move(loc), "Redefinition of: " + name),
    name(std::move(name)) {}
};

// Internal compiler error
class InternalCompilerError : public std::runtime_error {
public:
    InternalCompilerError(const std::string &msg) : std::runtime_error("Internal compiler error: " + msg) {}
};

class ErrorReporter {
public:
    [[noreturn]] void panic(const SourceLoc &loc, const std::string &expected, const std::string &found) {
        throw SyntaxError(loc, expected, found);
    }

    [[noreturn]] void typeError(const SourceLoc &loc, const std::string &expected, const std::string &found) {
        throw TypeError(loc, expected, found);
    }

    [[noreturn]] void undefinedVariable(const SourceLoc &loc, const std::string &varName) {
        throw UndefinedVariableError(loc, varName);
    }

    [[noreturn]] void redefinition(const SourceLoc &loc, const std::string &name) {
        throw RedefinitionError(loc, name);
    }
};

#endif // ERROR_REPORTED_H