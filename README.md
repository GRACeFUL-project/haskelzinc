# Description
This package was created in the context of the [GRACeFUL project](https://www.graceful-project.eu/).

The haskelzinc library defines an interface to the MiniZinc constraint proramming language. It provides

* a Haskell abstract syntax tree for the MiniZinc language, with which one can represent MiniZinc models in Haskell
* a human-friendly DSL for building MiniZinc model representations
* a pretty printer to print the representation of a MiniZinc model in MiniZinc
* a parser that returns a representation of the solutions obtained by running the MiniZinc model
* a set of functions useful for building a custom FlatZinc solutions parser.
* a set of functions for constructing a model with time and space constraints, including cost related constraints.

An additional module gives the possibility to directly get the solutions of a MiniZinc finite domain model. 
Option for interactive interface is provided, as well as choice between two solvers: the G12/FD built-in 
solver of FlatZinc and choco3.

# Documentation
API documentation is provided in [hackage](https://hackage.haskell.org/package/haskelzinc). Additionally, a 
[user manual](https://hackage.haskell.org/package/haskelzinc-0.3.1.0/src/haskelzinc_User_Guide.pdf) comes 
together with the installation of the package.

# Installation
This library is [available](https://hackage.haskell.org/package/haskelzinc) on hackage. Use `cabal install`.

## Requirements
 * GHC 7.10.3+ or 8
 * MiniZinc 2.0 or 2.1

## Optional requirements
To use choco solver, also required:

   - JDK 8+
   - The following jar files (can be also found in the `choco/` directory of [the haskelzinc repo](https://github.com/GRACeFUL-project/haskelzinc))
     - choco_solver (with dependencies) [http://choco-solver.org/Download?q=releases]
     - choco_parser [https://oss.sonatype.org/content/repositories/releases/org/choco-solver/choco-parsers/3.3.3/]
     - ANTLR >4.5.2 java runtime binaries [http://www.antlr.org/download.html]

# Configuration
  1. Create a file `HZconf/conf.txt` in the same directory level where you want to run your code.
  2. Fill in the corresponding paths by adding the equal sign (=) and the correct path.
    * MINIZINC_DIR: the directory where `mzn2fzn` and `flatzinc` executables are located
    * CHOCO_PARSER: the path of the choco parser java library
    * CHOCO_SOLVER: the path of the choco solver java library
    * ANTLR: the path of the antlr java library

Example:

> MINIZINC_DIR = path/to/dir

# Not yet supported
 * MiniZinc enumerated types
 * Set constraints with the choco solver

# Running a model
Consider the `nineDigitArrangement` which can be found in `GExamples.hs` (root directory).
One needs to import module `Interfaces.MZinHaskell` to use the functions `iRunModel` or `runModel` which run a given representation of a MiniZinc model and return its solution(s).

## Interactive
To run the model interactively, use `iRunModel` and follow the instructions. An example is given below. 

```
> iRunModel nineDigitArrangement
Enter working directory:
/path/to/desired/directory
Enter model's name: nine
Choose a solver from the list below:
        1. G12/FD
        2. choco3

Integer value associated with the solver: 1
Number of solutions to be returned: 1
Right [[("A",MInt 5),("B",MInt 3),("C",MInt 2),("D",MInt 1),("E",MInt 4),("F",MInt 7),("G",MInt 6),("H",MInt 9),("I",MInt 8),("s",MInt 7448)]]
```

The script first asks for a working directory. This is where related files will be stored. Next, a name for the model should be given, which will be used for naming the files that will be stored in the working directory. After a name is chosen, the user is presented with a list of supported solvers from which (s)he can choose by entering the corresponding number of the solver. Last, the user is prompted to specify the desired number of solutions. The number of returned solutions will be at most of the user given number. They will be less in case there are no more solutions.

## Noninteractive
The same functionality can be triggered in a noninteractive manner, with the use of `runModel`. The user input in `iRunModel` is entered as arguments (in the same order) of `runModel`. Make sure to escape special characters in the string of the working directory path. The example below returns the same results as the interactive example above.

```
runModel nineDigitArrangement "/path/to/desired/directory" "nine" 1 1
```
