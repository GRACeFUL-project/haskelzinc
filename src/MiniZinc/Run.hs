-----------------------------------------------------------------------------
-- Copyright 2017, GRACeFUL project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alexg@chalmers.se
-- Stability   :  experimental
-- Portability :  portable (depends on ghc)
--
-- Run a constraint programming model using a constraint solver.
--
-----------------------------------------------------------------------------

module MiniZinc.Run 
    ( -- * Solving 
      solve
      -- * Querying results
    , Result, getVar, readVar
    ) where

import MiniZinc.Constraint (Ident, Expr, getId)
import MiniZinc.Model

import Data.Maybe
import qualified Data.Map as M
import System.Directory
import System.IO
import System.Process
import Text.Parsec 

type Result = Maybe (M.Map Ident String)

solver = "mzn-g12mip"

solve :: Model -> IO Result
solve m = do
    (file, handle) <- openTempFile "." "model.mzn"
    hPutStr handle $ emit m
    hClose handle
    output <- readProcess solver [file] []
    removeFile file 
    return $ parseResult output

getVar :: Result -> Expr -> String
getVar res e = fromJust $ getId e >>= \n -> res >>= M.lookup n  

readVar :: Read a => Result -> Expr -> a
readVar res = read . getVar res

parseResult :: String -> Result
parseResult = either (const Nothing) (Just . M.fromList) . parse p "" 
  where
    p   = keyval `endBy` char '\n' <* suc
    suc = many1 (char '-')  -- a dashed line indicates success
    key = (:) <$> letter <*> many (letter <|> digit <|> char '_')
    val = many1 (noneOf " ;")
    keyval = do
        k <- key 
        spaces ; char '=' ; spaces 
        v <- val
        char ';'
        return (k, v)
