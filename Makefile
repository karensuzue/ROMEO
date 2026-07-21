# This is the Makefile for Avida
# To use, you must:
#  - setup EMP_DIR to indicate the location of the Empirical root directory.
#
# Build options:
#  native (default) - Optimized version of code.
#  debug - turn on all debugging options, including asserts and pointer tracking.
#  quick - no debugging or optimization; fastest compile time.
#  clean - remove all compilation artifacts including executable.
#  web - compile to WebAssembly with optimizations on (same as web-native)
#  web-debug - compile to WebAssembly with debug on (same as web-native)
#  web-quick - compile to WebAssembly with neither debug or optimization
#
# Other build options, less used:
#  grumpy - Lots of extra warnings turned on

TARGET := ROMEO

# Additional executables to build from source/<name>.cpp (each also gets a <name>-debug target)
ALTERNATES := DOSSIER Avida

# Identify all directory locations
EMP_DIR      = ../Empirical
BUILD_DIR    = build
WEB_DIR      = web
SETTINGS_DIR = settings
SOURCE_DIR   = source

# CXX = clang++
CXX_web := emcc

NATIVE_CODE = $(SOURCE_DIR)/$(TARGET).cpp
WEB_CODE = $(SOURCE_DIR)/$(TARGET)-web.cpp

NATIVE_EXE = $(BUILD_DIR)/$(TARGET)
WEB_EXE = $(WEB_DIR)/$(TARGET).js

# Specify sets of compilation flags to use
FLAGS_version := -std=c++23
FLAGS_warn    := -Wall -Wextra -Wno-unused-function -Wnon-virtual-dtor -Wcast-align -Woverloaded-virtual -pedantic
FLAGS_include := -I$(SOURCE_DIR)/ -I$(EMP_DIR)/include/
FLAGS_main    := $(FLAGS_version) $(FLAGS_warn) $(FLAGS_include) # -pthread

FLAGS_QUICK    := $(FLAGS_main) -DNDEBUG
FLAGS_DEBUG    := $(FLAGS_main) -g -DEMP_TRACK_MEM
# Not using opts:
# -flto (link-time optimization; we have only one compilation unit)
# -ffast-math (may cause issues with some floating-point calculations)
FLAGS_OPT      := $(FLAGS_main) -O3 -DNDEBUG -march=native -fno-exceptions
FLAGS_GRUMPY   := $(FLAGS_main) -DNDEBUG -Wconversion -Weffc++
FLAGS_COVERAGE := $(FLAGS_main)  -O0 -DEMP_TRACK_MEM -ftemplate-backtrace-limit=0 -fprofile-instr-generate -fcoverage-mapping -fno-inline -fno-elide-constructors

# Emscripten / Empirical information
EMP_methods  := -s EXPORTED_RUNTIME_METHODS="['ccall', 'cwrap', 'UTF8ToString', 'stringToUTF8', 'lengthBytesUTF8']"
EMP_funs     := -s EXPORTED_FUNCTIONS="['_main', '_malloc', '_free', '_empCppCallback']"
EMP_js_lib   := --js-library $(EMP_DIR)/include/emp/web/library_emp.js
EMP_limits   := -s NO_EXIT_RUNTIME=1  -s TOTAL_MEMORY=67108864
EMP_warnings := -Wno-dollar-in-identifier-extension
FLAGS_emp    := $(FLAGS_main) $(EMP_methods) $(EMP_js_lib) $(EMP_funs) $(EMP_limits) $(EMP_warnings)

FLAGS_WEB       := $(FLAGS_emp) -Oz -DNDEBUG -s DISABLE_EXCEPTION_CATCHING=1
FLAGS_WEB_DEBUG := $(FLAGS_emp) -gsource-map -pedantic -s ASSERTIONS=1
FLAGS_WEB_QUICK := $(FLAGS_emp) -DNDEBUG

native: FLAGS := $(FLAGS_OPT)
native: $(NATIVE_EXE)

default: native

debug: FLAGS := $(FLAGS_DEBUG)
debug: $(NATIVE_EXE)

grumpy: FLAGS := $(FLAGS_GRUMPY)
grumpy: $(NATIVE_EXE)

quick: FLAGS := $(FLAGS_QUICK)
quick: $(NATIVE_EXE)

web: FLAGS := $(FLAGS_WEB)
web: $(WEB_EXE)

web-debug: FLAGS := $(FLAGS_WEB_DEBUG)
web-debug: $(WEB_EXE)

web-quick: FLAGS := $(FLAGS_WEB_QUICK)
web-quick: $(WEB_EXE)

all: native web

new: clean
new: native

# Debugging information
#print-%: ; @echo $*=$($*)
print-%: ; @echo '$(subst ','\'',$*=$($*))'

CLEAN_BACKUP = *~ *.dSYM
CLEAN_TEST = *.out	*.o	*.gcda	*.gcno	*.info	*.gcov	./Coverage* ./temp
CLEAN_EXE = $(NATIVE_EXE) $(WEB_EXE) $(addprefix $(BUILD_DIR)/, $(ALTERNATES))

CLEAN_FILES = $(CLEAN_BACKUP) $(CLEAN_TEST) $(CLEAN_EXE)

server:
	cd $(WEB_DIR) ; python $(CURDIR)/web/serve.py

# Always run the tests, even if nothing has changed
.PHONY: clean debug grumpy native quick server tests web web-debug web-quick \
        $(ALTERNATES) $(addsuffix -debug, $(ALTERNATES))

# Changes in any header file in SOURCE_DIR should trigger recompilation
KEY_HEADERS := $(shell find $(SOURCE_DIR) -name '*.hpp')

clean:
	@echo Removing:
	@echo $(wildcard $(CLEAN_FILES))
	@echo ----
	rm -rf $(wildcard $(CLEAN_FILES))

# Make sure that the needed directories exists.
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)
$(WEB_DIR):
	mkdir -p $(WEB_DIR)

$(ALTERNATES): FLAGS := $(FLAGS_OPT)
$(ALTERNATES): % : $(BUILD_DIR)/%

$(addprefix $(BUILD_DIR)/, $(ALTERNATES)): $(BUILD_DIR)/% : $(SOURCE_DIR)/%.cpp $(KEY_HEADERS) | $(BUILD_DIR)
	$(CXX) $(FLAGS) $< -o $@

$(addsuffix -debug, $(ALTERNATES)): FLAGS := $(FLAGS_DEBUG)
$(addsuffix -debug, $(ALTERNATES)): %-debug : $(SOURCE_DIR)/%.cpp $(KEY_HEADERS) | $(BUILD_DIR)
	$(CXX) $(FLAGS) $< -o $(BUILD_DIR)/$*

# Compile the command-line version.
$(NATIVE_EXE): $(NATIVE_CODE) $(KEY_HEADERS) | $(BUILD_DIR)
	$(CXX) $(FLAGS) $(NATIVE_CODE) -o $(NATIVE_EXE)
	@echo To build the web version use: make web

# Compile the web version.
$(WEB_EXE): $(WEB_CODE) $(KEY_HEADERS) | $(WEB_DIR)
	$(CXX_web) $(FLAGS) $(WEB_CODE) -o $(WEB_EXE)
