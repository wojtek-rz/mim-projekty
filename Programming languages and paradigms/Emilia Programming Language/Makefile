CABAL := cabal
HASKEL_TARGET := emilia-lang-exe
EXECUTABLE_PATH := $(shell $(CABAL) list-bin $(HASKEL_TARGET))

all: build


build:
	$(CABAL) build
	@echo "Build successful!"
	@echo "Copying executable to current directory..."
	cp $(EXECUTABLE_PATH) .

clean:
	$(CABAL) clean
