# Hack Assembler

An assembler written in OCaml that translates Hack assembly language (`.asm`) into 16-bit binary Hack machine code (`.hack`), built as part of a computer architecture project following the Nand2Tetris course structure.

## Features

- **Two-pass assembly**: first pass resolves label symbols (e.g. `(LOOP)`) to instruction addresses; second pass encodes each instruction into binary
- **Symbol table**: handles predefined symbols (registers, `SCREEN`, `KBD`, etc.), labels, and user-defined variables
- **A-instruction parsing**: converts `@value` and `@symbol` instructions into 16-bit binary
- **C-instruction encoding**: parses `dest=comp;jump` syntax into the correct `comp`, `dest`, and `jump` bit fields
- **Label resolution**: strips whitespace/comments and resolves all jump/loop labels before final encoding

## Usage

```bash
dune build
dune exec bin/main.exe -- path/to/Program.asm
```

This produces `Program.hack` in the same directory, containing the binary machine code.

*(Update the exact executable name/path above to match your `bin/` entry point.)*

## Project Structure

```
.
├── bin/                # Executable entry point
├── lib/                # Core assembler logic
│   ├── ast.ml          # AST types for parsed instructions
│   ├── parser.ml       # Parses A- and C-instructions into the AST
│   └── machine.ml       # Translates AST into binary machine code
├── test/               # Test suite
├── assembler.opam      # OPAM package definition
├── dune-project         # Dune project configuration
└── .gitignore
```

## Background

This assembler is part of the Hack platform toolchain from the *Elements of Computing Systems* (Nand2Tetris) course, which builds a computer from the ground up — starting with logic gates and ending with a working assembler, VM translator, and compiler for a simple high-level language.

## Requirements

- OCaml (version X.X+)
- Dune build system
