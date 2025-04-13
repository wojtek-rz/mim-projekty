# Instant compiler

## Building the project

To build the `insc_llvm` and `insc_jvm` executable files, run the following command:
```bash
make
```

### Under the hood

Under the hood, the `Makefile` uses `cabal` to build the project with `cabal build`. 
Cabal looks for `instant.cabal` file for the configuration. 

The project is written 
with `stack` tool, so you can also use:
```bash
stack build
stack exec insc_llvm
```
To build and run the `insc_llvm` executable.

### Creating archive

In the main directory, run:
```
make archive
```

## Directory structure

```bash
.
├── app
├── examples
├── instant.cabal
├── Instant.cf
├── lib
├── Makefile
├── package.yaml
├── README.md
├── src
└── stack.yaml
```
### Directories

- `app` - contains the haskell code for the main modules of `insc_jvm`, `insc_llvm` (just the CLI interface)
- `examples` - contains the examples for the instant language
- `lib` - contains the `jasmin.jar` file for the jasmin assembler
- `src` - contains the code for the compilers
- `package.yaml` - the package file for the project
- `stack.yaml` - the stack file for the project
- `Makefile` - the makefile for the project
- `Instant.cf` - the BN grammar for the instant language

### Source code directory structure

```bash
.
├── Bnfc
├── JVMCompiler.hs
├── LLVMCompiler.hs
└── Utilities.hs
```

## Notes

Adding in `package.yaml`:
```bash
    other-modules:       []
```
to fix GHC errors with main module not being found.

To get the parser, run:
```bash
mv instant.cf bnfc.cf
bnfc -d -o output -m --functor bnfc.cf
```

Commands:

- `java -jar lib/jasmin.jar -d <output dir> <path to jasmin bc>`
- `lli <path to llvm bc>`