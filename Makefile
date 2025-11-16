# Define C++ compiler
CXX=clang++ -std=c++17

# --- Compiler and Linker Flags ---

# Define compile flags
# Get the C++ flags from llvm-config
# -g  - debug symbols
# -O3 - optimisation level
# -c  - compile only
# -Iinclude - look for headers in the 'include' directory
CPPFLAGS = -g -O3 `llvm-config --cppflags` -Iinclude \
		   -Wno-unused-function -Wno-unknown-warning-option -fno-rtti

# Define linker flags
LDFLAGS = `llvm-config --ldflags --system-libs --libs all`

# --- Project Files ---

# Build directory
BUILD_DIR = build

# Final executable name
TARGET = $(BUILD_DIR)/mccomp

# All .cpp source files are now in the 'src' directory
SRCS = $(wildcard src/*.cpp)

# All .h header files are now in the 'include' directory
HEADERS = $(wildcard include/*.h)

# Object files are derived from source files, but without the 'src/' prefix
OBJS = $(patsubst src/%.cpp,$(BUILD_DIR)/%.o,$(SRCS))

# --- Build Rules ---

# Default rule, "all", builds main target.
all: $(TARGET)

# Rule to link final executable (mccomp)
# This rule depends on all the object files (.o)
# Links using LDFLAGS
$(TARGET): $(OBJS) | $(BUILD_DIR)
	@echo "Linking $(TARGET)..."
	$(CXX) $(OBJS) -o $(TARGET) $(LDFLAGS)

# Pattern rule to compile a .cpp file from 'src' into a .o file in the root
# This rule now correctly finds sources in the src/ directory
# It also depends on all header files. A change in any header will recompile all sources.
$(BUILD_DIR)/%.o: src/%.cpp $(HEADERS) | $(BUILD_DIR)
	@echo "Compiling src/$*.cpp..."
	$(CXX) -c $< -o $@ $(CPPFLAGS)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# --- Utility Rules ---

# Rule to clean up all build files (the executable and all object files)
clean:
	@echo "Cleaning up..."
	rm -rf $(BUILD_DIR)

