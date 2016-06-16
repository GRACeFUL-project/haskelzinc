== REQUIREMENTS ==
 * GHC 7.10.3
 * MiniZinc 2.0
 * To use choco solver, also required:
   - JDK 8+
   - THe following jar files can be also found in the choco/ directory of this repo
     - choco_solver (with dependencies) [http://choco-solver.org/Download?q=releases]
     - choco_parser [https://oss.sonatype.org/content/repositories/releases/org/choco-solver/choco-parsers/3.3.3/]
     - ANTLR >4.5.2 java runtime binaries [http://www.antlr.org/download.html]

== INSTALLATION ==

  This library is available in hackage. Either download the source code and compile with runhaskell
  or install the package with cabal.

== USAGE ==

  1. Create a directory called "HZconf" in the directory of your Haskell code that calls testModel or iTestModel.
  2. In HZconf create a text file "conf.txt".
  3. Write the corresponding paths by adding the equal sign (=) and the correct path.
     MINIZINC_DIR: the directory where mzn2fzn and flatzinc executables are
     CHOCO_PARSER: the path of the choco parser java library
     CHOCO_SOLVER: the path of the choco solver java libaray
     ANTLR: the path of the antlr java libaray

     Example:
       MINIZINC_DIR = path/to/dir