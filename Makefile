# Define C++ compiler
CXX=clang++ -std=c++17

# --- Compiler and Linker Flags ---

# Define compile flags
# Get the C++ flags from llvm-config
# -g  - debug symbols
# -O3 - optimisation level
# -c  - compile only
CPPFLAGS = -g -O3 `llvm-config --cppflags` \
		   -Wno-unused-function -Wno-unknown-warning-option -fno-rtti

# Define linker flags
LDFLAGS = `llvm-config --ldflags --system-libs --libs all`

# --- Project Files ---

# Final executable name
TARGET = mccomp

# All .cpp source files
SRCS = main.cpp lexer.cpp parser.cpp ast.cpp codegen.cpp

# All .h header files 
HEADERS = lexer.h parser.h ast.h codegen.h

# Object files are derived from source files
OBJS = $(SRCS:.cpp=.o)

# --- Build Rules ---

# Default rule, "all", builds main target.
all: $(TARGET)

# Rule to link final executable (mccomp)
# This rule depends on all the object files (.o)
# Links using LDFLAGS
$(TARGET): $(OBJS)
	@echo "Linking $(TARGET)..."
	$(CXX) $(OBJS) -o $(TARGET) $(LDFLAGS)

# Pattern rule to compile a .cpp file into a .o file
# -c - compile only, do not link
# $< - the dependency
# $@ - the target
$.o: %.cpp $(HEADERS)
	@echo "Compiling$<..."
	$(CXX) -c $< -o $@ $(CPPFLAGS)

# --- Utility Rules ---

# Rule to clean up all build files (the executable and all object files)
clean:
	@echo "Cleaning up..."
	rm -rf $(TARGET) $(OBJS)

