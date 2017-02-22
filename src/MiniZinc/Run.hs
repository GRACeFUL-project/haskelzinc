module MiniZinc.Run 
    ( -- * Solving 
      solve
    , Result, getVar, readVar
    ) where

import Data.Maybe
import qualified Data.Map as M
import MiniZinc.Model
import System.Directory
import System.IO
import System.Process
import Text.Parsec 

type Result = Maybe (M.Map Ident String)

solve :: Model -> IO Result
solve m = do
    (file, handle) <- openTempFile "." "model.mzn"
    hPutStr handle $ emit m
    hClose handle
    output <- readProcess "mzn-g12mip" [file] []
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
