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
  module Interfaces.MZAST,
  module Interfaces.FZSolutionParser,
  module Interfaces.MZPrinter,
  iTestModel,
  testModel,
  testModelWithData,
  writeData
) where

import Interfaces.Auxiliary
import System.Process
#ifdef unix
import System.FilePath.Posix
#else
import System.FilePath.Windows
#endif
import Interfaces.MZPrinter
import Interfaces.FZSolutionParser
import Text.Parsec.Error
import Interfaces.MZAST hiding (UserD, PrefBop)

testModelWithData
  :: MZModel  -- The model
  -> MZModel  -- The data to be used by the model
  -> FilePath -- The path of the file in which the FlatZinc translation will be printed
  -> String   -- ^ The chosen solver ("fd" for the G12/FD built-in solver or empty string for choco3)
  -> String   -- ^ "0" for all solutions, empty string for the first solution
  -> IO (Either ParseError [Solution])
testModelWithData model mdata path solver num = 
  let fdata = [Comment "Model\'s data"] ++ mdata ++ [Empty]
  in testModel (mdata ++ model) path solver num

-- | Interactively runs a model and outputs its solution(s). The function first prompts the user
-- for the paths of the file in which the represented MiniZinc model will be printed and the 
-- data file if required. Then asks the user to choose between supported solvers and the desired 
-- number of solutions (only one or all supported for now). Finally, it uses the chosen solver 
-- and parses the solution(s).
iTestModel :: MZModel -> IO (Either ParseError [Solution])
iTestModel m = do
  putStrLn "Enter FlatZinc model's filepath:"
  path <- getLine
  putStrLn "Type \"fd\" for G12/FD solver or leave empty for choco solver."
  solver <- getLine
  putStrLn "Enter 0 to output all solutions."
  mode <- getLine
  testModel m path solver mode

-- | Runs a model and parses its solution(s).
testModel :: MZModel -- ^ The model
  -> FilePath         -- ^ The path of the file in which the FlatZinc translation will be printed
  -> String           -- ^ The chosen solver ("fd" for the G12/FD built-in solver or empty string for choco3)
  -> String           -- ^ "0" for all solutions, empty string for the first solution
  -> IO (Either ParseError [Solution])
testModel m mzn s n = do
  configuration <- parseConfig
  let (filename, _) = splitExtension mzn
  let mz_dir = case minizinc configuration of
                ""  -> addTrailingPathSeparator "."
                str -> addTrailingPathSeparator str
  let mfzn = (spaceFix $ mz_dir ++ "mzn2fzn") ++ " -O- - -o " ++ (spaceFix (filename ++ ".fzn"))
  let flatzinc = spaceFix $ mz_dir ++ "flatzinc"
  -- Uncomment line below for debugging only
  -- writeFile (filename ++ ".mzn") (Prelude.show $ printModel m)
  readCreateProcess (shell mfzn) (Prelude.show $ printModel m)
  let opt = case n of
              "0" -> " -a "
              _   -> " "
  res <- case s of
          "" -> let antlr = antlr_path configuration
                    chocoParser = chocoparser configuration
                    chocoSolver = chocosolver configuration
                in readCreateProcess (shell $ "java -cp ." ++ [searchPathSeparator] ++ chocoSolver ++ [searchPathSeparator] ++ chocoParser ++ [searchPathSeparator] ++ antlr ++ " org.chocosolver.parser.flatzinc.ChocoFZN" ++ opt ++ filename ++ ".fzn") ""
          _  -> readCreateProcess (shell $ flatzinc ++ opt ++ "-b fd " ++ filename ++ ".fzn") ""
  return $ getSolution res

-- | Writes the model's data file. The 'MZModel' of the argument must contain
-- only 'Assignment' items.
writeData :: MZModel -> IO ()
writeData m = do
  putStrLn "Enter MiniZinc datafile's filepath:"
  datapath <- getLine
  writeFile datapath (Prelude.show $ printModel m)
