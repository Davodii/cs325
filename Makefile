
# Define C++ compiler
CXX=clang++ -std=c++17

# --- Build Mode ---
MODE = debug
# MODE = asan
# MODE = release

# --- Base compiler + linker flags ---

LLVM_CPPFLAGS := $(shell llvm-config --cxxflags)
LLVM_LDFLAGS := $(shell llvm-config --ldflags --system-libs --libs all)

BASE_CPPFLAGS = -Iinclude -Wno-unused-function -Wno-unknown-warning-option -fno-rtti \
                -fexceptions

# --- Mode-specific flags ---
ifeq ($(MODE), debug)
	CPPFLAGS = -g -O0 $(LLVM_CPPFLAGS) $(BASE_CPPFLAGS)
	LDFLAGS = $(LLVM_LDFLAGS)
endif

ifeq ($(MODE), asan)
	export ASAN_OPTIONS=detect_leaks=0

	CPPFLAGS = -g -O1 $(LLVM_CPPFLAGS) $(BASE_CPPFLAGS) \
				-fsanitize=address -fsanitize-address-use-after-scope -fno-omit-frame-pointer \
				-fsanitize-recover=address
	LDFLAGS = -fsanitize=address $(LLVM_LDFLAGS)
endif

ifeq ($(MODE), release)
	CPPFLAGS = -O3 $(LLVM_CPPFLAGS) $(BASE_CPPFLAGS)
	LDFLAGS = $(LLVM_LDFLAGS)
endif

# --- Project Files ---

# Build directory
BUILD_DIR = build

# Final executable name
# TARGET = $(BUILD_DIR)/mccomp
TARGET = mccomp

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

