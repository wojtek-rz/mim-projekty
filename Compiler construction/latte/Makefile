CABAL := cabal
HASKEL_TARGET := latc_llvm
EXECUTABLE_PATH := $(shell $(CABAL) list-bin $(HASKEL_TARGET))

all: build

build:
	$(CABAL) build
	@echo "Build successful!"
	@echo "Copying executable to current directory..."
	cp $(EXECUTABLE_PATH) .

clean:
	$(CABAL) clean

archive: clean
	@echo "Creating archive..."
	@tar -czf wojciech-rzeplinski-latte.tgz src/ app/ lib/ Makefile latte.cabal package.yaml stack.yaml README.md