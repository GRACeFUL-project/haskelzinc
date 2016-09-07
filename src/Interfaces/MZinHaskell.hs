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
import System.FilePath
import Interfaces.MZPrinter
import Interfaces.FZSolutionParser
import Text.Parsec.Error
import Interfaces.MZAST hiding (UserD, PrefBop)

testModelWithData
  :: MZModel  -- ^ The model
  -> MZModel  -- ^ The data to be used by the model
  -> FilePath -- ^ The path of the file in which the FlatZinc translation will be printed
  -> Int      -- ^ The chosen solver ("fd" for the G12/FD built-in solver or empty string for choco3)
  -> Int      -- ^ 0 for all solutions
  -> IO (Either ParseError [Solution])
testModelWithData model mdata path solver num = 
  let fdata = [Comment "Model\'s data"] ++ mdata ++ [Empty]
  in testModel (fdata ++ model) path solver num

-- | Interactively runs a model and outputs its solution(s). The function first prompts the user
-- for the paths of the file in which the represented MiniZinc model will be printed and the 
-- data file if required. Then asks the user to choose between supported solvers and the desired 
-- number of solutions (only one or all supported for now). Finally, it uses the chosen solver 
-- and parses the solution(s).
iTestModel :: MZModel -> IO (Either ParseError [Solution])
iTestModel m = do
  putStrLn "Enter working directory:"
  dirpath <- getLine
  putStr "Enter model\'s name: "
  name <- getLine
  putStr "Choose a solver from the list below:\r\n\t1. G12/FD\r\n\t2. choco3\r\n\r\nInteger value associated with the solver: "
  str_solver <- getLine
  putStr "Number of solutions to be returned: "
  str_ns <- getLine
  let solver = read str_solver
      ns = read str_ns
      path = joinPath [dirpath, name]
  testModel m path solver ns

-- | Runs a model and parses its solution(s).
testModel :: MZModel -- ^ The model
  -> FilePath        -- ^ The path of the file in which the FlatZinc translation will be printed (without ".fzn" extension)
  -> Int             -- ^ The chosen solver ("fd" for the G12/FD built-in solver or empty string for choco3)
  -> Int             -- ^ 0 for all solutions
  -> IO (Either ParseError [Solution])
testModel m mpath s n = do
  configuration <- parseConfig
  let mz_dir = case minizinc configuration of
                ""  -> addTrailingPathSeparator "."
                str -> addTrailingPathSeparator str
  let mfzn = (spaceFix $ mz_dir ++ "mzn2fzn") ++ " -O- - -o " ++ (spaceFix (mpath ++ ".fzn"))
  let flatzinc = spaceFix $ mz_dir ++ "flatzinc"
  -- Uncomment line below for debugging only
  -- writeFile (mpath ++ ".mzn") (Prelude.show $ printModel m)
  readCreateProcess (shell mfzn) (Prelude.show $ printModel m)
  let opt = case n of
              0 -> " -a "
              _ -> " "
  res <- case s of
           1 -> readCreateProcess (shell $ flatzinc ++ opt ++ "-b fd " ++ mpath ++ ".fzn") ""
           2 -> let antlr       = antlr_path configuration
                    chocoParser = chocoparser configuration
                    chocoSolver = chocosolver configuration
                in readCreateProcess (shell $ "java -cp ." ++ [searchPathSeparator] ++ chocoSolver ++ [searchPathSeparator] ++ chocoParser ++ [searchPathSeparator] ++ antlr ++ " org.chocosolver.parser.flatzinc.ChocoFZN" ++ opt ++ filename ++ ".fzn") ""
  return $ getSolution res

-- | Writes the model's data file. The 'MZModel' of the argument must contain
-- only 'Assignment' items.
writeData :: MZModel -> IO ()
writeData m = do
  putStrLn "Enter MiniZinc datafile's filepath:"
  datapath <- getLine
  writeFile datapath (Prelude.show $ printModel m)
