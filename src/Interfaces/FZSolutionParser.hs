{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : FZSolutionParser
Description : FlatZinc solutions parser
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module parses the solutions outputed by the specified FlatZinc solver. It supports multiple solutions.
The parser might fail if there is a show item in the represented MiniZinc model which alters the default
format of the solutions' output.

This parser is built using the "Text.Parsec" module.
-}

module Interfaces.FZSolutionParser (
  getSolution,
  Solutions
) where

import Data.Char
import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as C1
import Text.Parsec.String (Parser)

{-
  First part of code gets a list of string pairs, representing the solution of a model.
  In each pair, the first component is the variable's name and the second component
  represents its value.
-}

type Solution = [(String,String)]
type Solutions = [M.Map String MValue]

-- | Given the path of the file where the solution(s) have been printed, this function reads the file,
-- parses the solution(s) and prints them.

getSolution :: FilePath -> IO [M.Map String MValue]
getSolution path = do
  output <- readFile path
  let sol = map parse $ getPairs output
  return sol

getPairs :: String -> [Solution]
getPairs = groupBySolution . usefull . map clean . lines

groupBySolution :: [String] -> [Solution]
groupBySolution = map makePairs . groupString

groupString :: [String] -> [[String]]
groupString [] = []
groupString ["=====UNSATISFIABLE====="] = [["Unsatisfiable"]]
groupString cls = let dw = dropWhile (/= "----------") cls
                  in takeWhile (/= "----------") cls :
                     if null dw
                     then []
                     else groupString $ tail dw

makePairs :: [String] -> Solution
makePairs []      = []
makePairs ("Unsatisfiable":ls) = [("Unsatisfiable", "Unsatisfiable")]
makePairs (l:ls)  = let (name, value) = fmap tail $ break ('=' ==) l
                    in (name, value) : makePairs ls

usefull :: [String] -> [String]
usefull []                 = []
usefull ("":ls)            = usefull ls
usefull (('%':rs):ls)      = usefull ls
usefull ("==========":ls)  = usefull ls
usefull (l:ls)             = l : usefull ls

clean :: String -> String
clean = filter (not . (`elem` [' ', ';']))



{-
  Second part of code defines a parser.
-}

data MValue = MError String
            | MInt Int
            | MFloat Float
            | MBool Bool
            | MString String
            | MArray [MValue]
            | MSet (S.Set MValue)
  deriving Show

runParser :: Parser a -> String -> Either P.ParseError a
runParser p = P.parse (p <* C1.eof) ""

varName :: Parser String
varName = manyTill anyChar (char '=')

digit :: Parser Char
digit = C.digit

anyChar :: Parser Char
anyChar = C.anyChar

char :: Char -> Parser Char
char = C.char

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy = C1.sepBy

sepEndBy1 :: Parser a -> Parser b -> Parser [a]
sepEndBy1 = C1.sepEndBy1

-- count :: Int -> Parser a -> Parser [a]
-- count = C1.count

between :: Parser a -> Parser b -> Parser c -> Parser c
between = C1.between

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill = C1.manyTill

many1 :: Parser a -> Parser [a]
many1 = C1.many1

digitValue :: Parser Int
digitValue = do
  d <- digit
  return $ ord d - ord '0'

ascendDecimal =
  return $ \x y -> x*10 + y

opposite = do
  n <- natural
  return (-n)

natural :: Parser Int
natural = C1.chainl1 digitValue ascendDecimal

int :: Parser Int
int = (char '-' >> opposite ) <|> natural

string :: String -> Parser String
string = C.string

bool :: Parser Bool
bool = string "true" >> return True <|> (string "false" >> return False)

float :: Parser Float
float = do
  ipart <- many1 digit
  char '.'
  dpart <- many1 digit
  let a = read (ipart ++ "." ++ dpart) :: Float in
    return a

set :: Parser a -> Parser [a]
set p = between (char '{') (char '}') (sepBy p (string ","))

intM :: Parser MValue
intM = MInt <$> int

boolM :: Parser MValue
boolM = MBool <$> bool

floatM :: Parser MValue
floatM = MFloat <$> float

stringM :: Parser MValue
stringM = MString <$> P.many anyChar

setM :: Parser MValue -> Parser MValue
setM p = MSet . S.fromDistinctAscList <$> set p

indexRange :: Parser Int
indexRange = do
  a <- int
  string ".."
  b <- int
  return (b - a + 1)

arraySizes :: Parser [Int]
arraySizes = sepEndBy1 indexRange (string ",")

extract :: Parser MValue -> Parser [MValue]
extract p = between (char '[') (char ']') (sepBy p (string ","))

fixDims :: [Int] -> [MValue] -> MValue
fixDims []  _  = MError "Array dimensions error: fixDims applied on empty list"
fixDims [d] ms = MArray ms
fixDims ds  ms = fixDims (init ds) (fix1Dim (last ds) ms)

fix1Dim :: Int -> [MValue] -> [MValue]
fix1Dim _ [] = []
fix1Dim d ms = MArray (take d ms) : fix1Dim d (drop d ms)

array :: Parser MValue -> Parser MValue
array p = do
  string "array"
  manyTill anyChar (char '(')
  ls <- arraySizes
  es <- extract p
  string ")"
  return (fixDims ls es)

scalar :: Parser MValue
scalar = P.try floatM <|> intM <|> boolM <|> stringM

parser :: Parser MValue
parser = P.try floatM <|> intM <|> boolM <|> setM scalar <|> array scalar <|> stringM

parse :: Solution -> M.Map String MValue
parse [] = M.empty
parse (l:ls) =
  if fst l == "Unsatisfiable"
  then M.insert "Unsatisfiable" (MError "Unsatisfiable") M.empty
  else let value = runParser parser (snd l) in
         case value of
           Right v -> M.insert (fst l) v (parse ls)
           Left _  -> M.insert (fst l) (MError "General parse error") (parse ls)

-- for testing purposes
parseWithLeftOver :: Parser a -> String -> Either P.ParseError (a,String)
parseWithLeftOver p = P.parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill C1.anyToken C1.eof
