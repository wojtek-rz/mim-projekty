CABAL := cabal
LLVM_TARGET := insc_llvm
JVM_TARGET := insc_jvm
INTERPRETER_TARGET := interpreter

# Build both targets
all: insc_llvm insc_jvm

insc_llvm:
	$(CABAL) build $(LLVM_TARGET)
	@echo "Build $(LLVM_TARGET) successful!"
	@echo "Copying $(LLVM_TARGET) executable to current directory..."
	cp $(shell $(CABAL) list-bin $(LLVM_TARGET)) .

insc_jvm:
	$(CABAL) build $(JVM_TARGET)
	@echo "Build $(JVM_TARGET) successful!"
	@echo "Copying $(JVM_TARGET) executable to current directory..."
	cp $(shell $(CABAL) list-bin $(JVM_TARGET)) .

interpreter:
	$(CABAL) build $(INTERPRETER_TARGET)
	@echo "Build $(INTERPRETER_TARGET) successful!"
	@echo "Copying $(INTERPRETER_TARGET) executable to current directory..."
	cp $(shell $(CABAL) list-bin $(INTERPRETER_TARGET)) .

archive: clean
	@echo "Creating archive..."
	@tar -czf insc.tar.gz src/ app/ lib/ examples/ Makefile instant.cabal package.yaml stack.yaml README.md

clean:
	$(CABAL) clean
	rm -rf dist-newstyle
	rm -f $(LLVM_TARGET) $(JVM_TARGET) $(INTERPRETER_TARGET) 
