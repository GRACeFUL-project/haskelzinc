# Description

This package was created in the context of the [GRACeFUL project](https://www.graceful-project.eu/).

The haskelzinc library defines an interface to the MiniZinc constraint proramming language. It provides 
* a Haskell abstract syntax tree for the MiniZinc language, with which one can represent MiniZinc models in Haskell
* a human-friendly DSL for building MiniZinc model representations
* a pretty printer to print the representation of a MiniZinc model in MiniZinc
* a parser that returns a representation of the solutions obtained by running the MiniZinc model
* a set of functions useful for building a custom FlatZinc solutions parser.

An additional module gives the possibility to directly get the solutions of a MiniZinc finite domain model. Option for interactive interface is provided, as well as choice between two solvers: the G12/FD built-in solver of FlatZinc and choco3.

# Requirements

 * GHC 7.10.3 or 8
 * MiniZinc 2.0 or 2.1

## Optional

To use choco solver, also required:

   - JDK 8+
   - The following jar files (can be also found in the choco/ directory of [the haskelzinc repo](https://github.com/GRACeFUL-project/haskelzinc))
     - choco_solver (with dependencies) [http://choco-solver.org/Download?q=releases]
     - choco_parser [https://oss.sonatype.org/content/repositories/releases/org/choco-solver/choco-parsers/3.3.3/]
     - ANTLR >4.5.2 java runtime binaries [http://www.antlr.org/download.html]

# Installation
  This library is [available](https://hackage.haskell.org/package/haskelzinc) on hackage. Use `cabal install`.

# Configuration

  1. Create a file `HZconf/conf.txt` in the same directory level where you want to run your code.
  2. Fill in the corresponding paths by adding the equal sign (=) and the correct path.
    
    * MINIZINC_DIR: the directory where mzn2fzn and flatzinc executables are located
    * CHOCO_PARSER: the path of the choco parser java library
    * CHOCO_SOLVER: the path of the choco solver java libaray
    * ANTLR: the path of the antlr java libaray

Example:

       MINIZINC_DIR = path/to/dir

# Not yet supported

 * MiniZinc enumerated types
 * Set constraints with the choco solver