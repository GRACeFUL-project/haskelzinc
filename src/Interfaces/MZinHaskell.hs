{-|
Module      : MZinHaskell
Description : Integration of MiniZinc 2.0 in Haskell
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module integrates constraint solving programming through MiniZinc in Haskell.
-}

{-# LANGUAGE CPP #-}

module Interfaces.MZinHaskell (
  module AST,
  module Parse,
  module Print,
  iTestModel,
  testModel,
  writeData
) where

import System.Directory
import System.Process
import System.FilePath.Posix
import qualified Interfaces.Auxiliary         as Aux
import qualified Interfaces.MZPrinter         as Print
import qualified Interfaces.FZSolutionParser  as Parse
import qualified Interfaces.MZAST             as AST -- hiding (UserD, PrefBop)

-- | Interactively runs a model and outputs its solution(s). The function first prompts the user
-- for the paths of the file in which the represented MiniZinc model will be printed and the
-- data file if required. Then asks the user to choose between supported solvers and the desired
-- number of solutions (only one or all supported for now). Finally, it uses the chosen solver
-- and parses the solution(s).
iTestModel :: AST.MZModel -> IO Parse.Solutions
iTestModel m = do
  putStrLn "Enter MiniZinc model's filepath:"
  path <- getLine
  putStrLn "Is there a data file? If yes, provide its filepath:"
  dpath <- getLine
  putStrLn "Type \"fd\" for G12/FD solver or leave empty for choco solver."
  solver <- getLine
  putStrLn "Enter 0 to output all solutions."
  mode <- getLine
  testModel m path dpath solver mode

fzn_name = "fzn-gecode"

-- | Runs a model and parses its solution(s).
testModel :: AST.MZModel -- ^ The model
  -> FilePath         -- ^ The path of the file in which the represented MiniZinc model will be printed
  -> FilePath         -- ^ The path of the data file if required, else an empty string
  -> String           -- ^ The chose solver ("fd" for the G12/FD built-in solver or empty string for choco3)
  -> String           -- ^ "0" for all solutions, empty string for the first solution
  -> IO Parse.Solutions
testModel m mzn' dtf' s n = do
  dir <- getCurrentDirectory
  let mzn = dir++"/"++mzn'
  let dtf = if not (null dtf') then dir++"/"++dtf' else ""
  configuration <- Aux.parseConfig
  let mz_dir = case Aux.minizinc configuration of
                ""  -> addTrailingPathSeparator "."
                str -> addTrailingPathSeparator str
  let mfzn     = Aux.spaceFix $ mz_dir ++ "mzn2fzn"
  let flatzinc = Aux.spaceFix $ mz_dir ++ fzn_name
  writeFile mzn (Prelude.show $ Print.printModel m)
  readCreateProcess (shell (mfzn ++" "++ mzn ++ " "++ dtf)) ""
  let (filename, _) = splitExtension mzn
  let opt = case n of
              "0" -> " -a "
              _   -> " "
  res <- case s of
          "" -> let antlr       = Aux.antlr_path  configuration
                    chocoParser = Aux.chocoparser configuration
                    chocoSolver = Aux.chocosolver configuration
                in readCreateProcess (shell $ "java -cp ." ++ [searchPathSeparator] ++ chocoSolver ++ [searchPathSeparator] ++ chocoParser ++ [searchPathSeparator] ++ antlr ++ " org.chocosolver.parser.flatzinc.ChocoFZN" ++ opt ++ filename ++ ".fzn> " ++ filename ++ ".fzn.results.txt") ""
          _  -> readCreateProcess (shell $ flatzinc ++ opt ++ filename ++ ".fzn > " ++ filename ++ ".fzn.results.txt") ""
  Parse.getSolution $ filename ++ ".fzn.results.txt"

-- | Writes the model's data file. The 'MZModel' of the argument must contain
-- only 'Assignment' items.
writeData :: AST.MZModel -> IO ()
writeData m = do
  putStrLn "Enter MiniZinc datafile's filepath:"
  datapath <- getLine
  writeFile datapath (Prelude.show $ Print.printModel m)
