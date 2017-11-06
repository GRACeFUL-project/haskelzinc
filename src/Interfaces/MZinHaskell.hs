{-|
Module      : MZinHaskell
Description : Integration of MiniZinc 2.0 in Haskell
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module provides IO functionality for running the Haskell representation of a
MiniZinc model and getting back the solutions in Haskell values.
-}

module Interfaces.MZinHaskell (
--  iTestModel,
  iRunModel,
  runModel,
  Interfaces.MZPrinter.layout,
--  testModelWithData,
--  testModelWithParser,
  writeData
) where

import Data.List
import System.Process
import System.FilePath
import Interfaces.MZAuxiliary
import Interfaces.MZASTBase (MZModel, Item(Comment))
import Interfaces.MZAST (GItem(..))
import Interfaces.MZPrinter
import Interfaces.FZSolutionParser (Solution, tryDefaultSolutions, getSolutions)
import Text.Parsec.Error
import Text.Parsec.String (Parser)
{-
-- | Same as `testModel` but accepts one more argument for the data of the model.
testModelWithData
  :: [GItem 'OK]  -- ^ The model
  -> [GItem 'OK]  -- ^ The data to be used by the model
  -> FilePath -- ^ Path of the file in which the FlatZinc translation will be printed (without ".fzn" extension)
  -> Int      -- ^ Chosen solver (@1@ for the G12/FD built-in solver or @2@ for choco3)
  -> Int      -- ^ Number of solutions to be returned
  -> IO (Either ParseError [Solution])
testModelWithData model mdata path solver num = 
  let fdata = [Comment' "Model\'s data"] ++ mdata ++ [Comment' "End of model\'s data"]
  in testModel (fdata ++ model) path solver num
-}
-- | Same as `testModel`, but interactive.
-- 
-- Interactively runs a constraint model and outputs its solution(s). The 
-- function first prompts the user for the working directory: the FlatZinc file 
-- will be created in that directory. Then, for a name for the constraint model: 
-- the created FlatZinc file will be named after this. Also asks the user to 
-- choose between supported solvers and the desired number of solutions. Returns 
-- either a parse error or a list of solutions of the constraint model. The length 
-- of the list is at most equal to the number of solutions requested.
iRunModel :: [GItem a] -> IO (Either ParseError [Solution])
iRunModel m = do
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
  runModel m path solver ns

-- | Runs a model and parses its solution(s). Use this function if the model contains no
-- @output@ item, so that the solutions have the default format.
runModel :: [GItem a] -- ^ The model
  -> FilePath        -- ^ The path of the file in which the FlatZinc translation will be printed (without ".fzn" extension)
  -> Int             -- ^ The chosen solver (@1@ for the G12/FD built-in solver or @2@ for choco3)
  -> Int             -- ^ The number of solutions to be returned
  -> IO (Either ParseError [Solution])
runModel = testModelWithParser tryDefaultSolutions

-- | Runs a model and parses its solution(s) with the use of the specified parser. Use
-- this function if the model outputs its solutions in a different format than the 
-- default.
testModelWithParser :: (Int -> Parser [Solution]) -- ^ The parser with which solutions will be
                                         -- parsed
                    -> [GItem a]         -- ^ The model
                    -> FilePath          -- ^ The path of the file in which the FlatZinc 
                                         -- translation will be printed (without ".fzn" 
                                         -- extension)
                    -> Int               -- ^ The chosen solver (@1@ for the G12/FD 
                                         -- built-in solver or @2@ for choco3)
                    -> Int               -- ^ The number of solutions to be returned
                    -> IO (Either ParseError [Solution])
testModelWithParser p m mpath s n = do
  configuration <- parseConfig
  let mz_dir = case minizinc configuration of
                ""  -> addTrailingPathSeparator "."
                str -> addTrailingPathSeparator str
  let mfzn = (spaceFix $ mz_dir ++ "mzn2fzn") ++ " -O- - -o " ++ (spaceFix (mpath ++ ".fzn"))
  let flatzinc = spaceFix $ mz_dir ++ "flatzinc"
  -- Uncomment line below for debugging
  writeFile (mpath ++ ".mzn") (layout m)
  readCreateProcess (shell mfzn) (layout m)
  res <- case s of
           1 -> readCreateProcess (shell $ flatzinc ++ " -a -b fd " ++ mpath ++ ".fzn") ""
           -- 1 -> readCreateProcess (shell $ flatzinc ++ " -a -b fd " ++ mpath ++ ".fzn > " ++ mpath ++ ".results.txt") ""
           2 -> let antlr       = antlr_path configuration
                    chocoParser = chocoparser configuration
                    chocoSolver = chocosolver configuration
                in readCreateProcess (shell $ "java -cp ." ++ (intercalate [searchPathSeparator] [chocoSolver, chocoParser, antlr]) ++ " org.chocosolver.parser.flatzinc.ChocoFZN -a " ++ mpath ++ ".fzn") ""
                -- in readCreateProcess (shell $ "java -cp ." ++ (intercalate [searchPathSeparator] [chocoSolver, chocoParser, antlr]) ++ " org.chocosolver.parser.flatzinc.ChocoFZN -a " ++ mpath ++ ".fzn > " ++ mpath ++ ".results.txt") ""
  -- Uncomment two lines below for debugging
  -- writeFile (mpath ++ ".results.txt") res
  -- getSolutionsFromFile (mpath ++ ".fzn.results.txt") n
  return $ getSolutions p n res

-- | Writes the model's data file. The 'MZModel' of the argument must contain
-- only 'Interfaces.MZASTBase.Assign' items.
writeData :: MZModel -> IO ()
writeData m = do
  putStrLn "Enter datafile's filepath:"
  datapath <- getLine
  writeFile datapath (Prelude.show $ printModel m)
