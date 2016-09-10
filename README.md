# Requirements
 * GHC 7.10.3
 * MiniZinc 2.0
## Optional
To use choco solver, also required:
   - JDK 8+
   - The following jar files (can be also found in the choco/ directory of [the haskelzinc repo](https://github.com/GRACeFUL-project/haskelzinc))
     - choco_solver (with dependencies) [http://choco-solver.org/Download?q=releases]
     - choco_parser [https://oss.sonatype.org/content/repositories/releases/org/choco-solver/choco-parsers/3.3.3/]
     - ANTLR >4.5.2 java runtime binaries [http://www.antlr.org/download.html]

Compatible with Windows and Unix systems.

# Configuration

  1. Create a file `HZconf/conf.txt` in the same directory level where you want to run your code.
  2. Fill in the corresponding paths by adding the equal sign (=) and the correct path.
    
    MINIZINC_DIR: the directory where mzn2fzn and flatzinc executables are located
    CHOCO_PARSER: the path of the choco parser java library
    CHOCO_SOLVER: the path of the choco solver java libaray
    ANTLR: the path of the antlr java libaray

Example:
       MINIZINC_DIR = path/to/dir

# Not yet supported
 * MiniZinc annotations
 * Set constraints with the choco solver