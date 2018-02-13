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
import Data.Char
import System.Process
import System.FilePath
import System.Directory
import Interfaces.MZAuxiliary
import Interfaces.MZASTBase (MZModel, Item(Comment))
import Interfaces.MZAST (GItem(..))
import Interfaces.MZPrinter
import Interfaces.FZSolutionParser (Solution, trySolutionsDefault, getAllSolutions)
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
  putStr "Choose a solver from the list below:\r\n\t1. G12/FD\r\n\t2. choco3\r\n\r\nEnter the number associated with the solver: "
  str_solver <- getLine
  putStr $ if (str_solver /= "2")
           then "Number of solutions to be returned: "
           else "Return all solutions? Y/N: "
  str_ns <- getLine;
  let solver = read str_solver
      ns = if (solver == 2)
           then if (read ("\"" ++ str_ns ++ "\"") == "Y")
                then 0
                else 1
           else read str_ns
      path = joinPath [dirpath, name]
  runModel m path solver ns
  
  
-- | Runs a model and parses its solution(s). Use this function if the model contains no
-- @output@ item, so that the solutions have the default format.
runModel :: [GItem a] -- ^ The model
  -> FilePath        -- ^ The path of the file in which the FlatZinc translation will be printed (without ".fzn" extension)
  -> Int             -- ^ The chosen solver (@1@ for the G12/FD built-in solver or @2@ for choco3)
  -> Int             -- ^ The number of solutions to be returned
  -> IO (Either ParseError [Solution])
runModel = testModelWithParser trySolutionsDefault

-- | Runs a model and parses its solution(s) with the use of the specified parser. Use
-- this function if the model outputs its solutions in a different format than the 
-- default.
testModelWithParser :: Parser [Solution] -- ^ The parser with which solutions will be
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
  -- Get configuration and set filepaths
  configuration <- parseConfig
  let mz_dir = addTrailingPathSeparator $ case minizinc configuration of
                                          ""  ->  "."
                                          str -> str
  let mzn_fp = spaceFix $ mpath ++ ".mzn"
  let fzn_fp = spaceFix $ mpath ++ ".fzn"
  let res_fp = spaceFix $ mpath ++ ".res"
  -- Write mzn file
  writeFile mzn_fp (layout m)
  let mzn2fzn  = proc (mz_dir ++ "mzn2fzn") ["-O-"
                                            ,"-o", fzn_fp
                                            , mzn_fp]
  (ec1, out1, err1) <- readCreateProcessWithExitCode mzn2fzn ""
  res <- case err1 of
         "" -> case s of
               -- G12/FD solver
               1 -> do
                    let fz_options = ["-b", "fd"]
                                     ++ case (n > 0) of
                                        True -> ["-n", show n]
                                        _    -> []
                                     ++ [fzn_fp]
                    let flatzinc = proc (mz_dir ++ "flatzinc") fz_options
                    (ec2, out2, err2) <- readCreateProcessWithExitCode flatzinc ""
                    return $ case err2 of
                             "" -> out2
                             _  -> "flatzinc error: " ++ err2 ++ "."
               -- Choco solver
               2 -> let antlr        = antlr_path configuration
                        chocoParser  = chocoparser configuration
                        chocoSolver  = chocosolver configuration
                        all_or_first = if (n == 0) then "-a " else ""
                    in readCreateProcess (shell $ "java -cp ." ++ (intercalate [searchPathSeparator] [chocoSolver, chocoParser, antlr]) ++ " org.chocosolver.parser.flatzinc.ChocoFZN " ++ all_or_first ++ mpath ++ ".fzn") ""
         _  -> readIO ("mzn2fzn error: " ++ err1 ++ ".")
  writeFile res_fp res
  -- Comment lines below for debugging
  removeFile res_fp
  removeFile mzn_fp
  removeFile fzn_fp
  return $ getAllSolutions p res

-- | Writes the model's data file. The 'MZModel' of the argument must contain
-- only 'Interfaces.MZASTBase.Assign' items.
writeData :: MZModel -> IO ()
writeData m = do
  putStrLn "Enter datafile's filepath:"
  datapath <- getLine
  writeFile datapath (Prelude.show $ printModel m)
