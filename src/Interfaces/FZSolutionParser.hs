{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

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
  printSolution,
  getSolutionFromFile,
  printSolutionFromFile,
  Solution
) where

import Data.Char
import Control.Applicative
import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as C1
import Text.Parsec.String (Parser)
-- Next two modules for testing only
import GHC.Generics
import Control.DeepSeq

type Solution = [(String, MValue)]

data MValue = MError String
            | MInt Int
            | MFloat Float
            | MBool Bool
            | MString String
            | MArray [MValue]
            | MSet (S.Set MValue)
  deriving (Show, Generic, NFData)

-- | Given the path of the file where the solution(s) have been printed, this function reads the file,
-- parses the solution(s) and returns them.

getSolutionFromFile :: FilePath -> IO (Either P.ParseError [Solution])
getSolutionFromFile path = do
  output <- readFile path
  return $ runParser trySolutions output

-- | Given the path of the file that constaint the solution(s), this function reads the file,
-- parses the solution(s) and prints them.

printSolutionFromFile :: FilePath -> IO ()
printSolutionFromFile path = do
  output <- readFile path
  print $ runParser trySolutions output

getSolution :: String -> Either P.ParseError [Solution]
getSolution = runParser trySolutions

printSolution :: String -> IO ()
printSolution = print . (runParser trySolutions)

-- Auxiliary definitions
digit :: Parser Char
digit = C.digit
  
anyChar :: Parser Char
anyChar = C.anyChar

char :: Char -> Parser Char
char = C.char

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy = C1.sepBy

between :: Parser a -> Parser b -> Parser c -> Parser c
between = C1.between

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill = C1.manyTill

many1 :: Parser a -> Parser [a]
many1 = C1.many1

eof :: Parser ()
eof = C1.eof

endOfLine :: Parser Char
endOfLine = C.endOfLine

string :: String -> Parser String
string = C.string

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse

try :: Parser a -> Parser a
try = P.try
-----------------------

runParser :: Parser a -> String -> Either P.ParseError a
runParser p = parse (p <* eof) ""

trySolutions :: Parser [Solution]
trySolutions = (try solutions <|> (unsat >> return [[]])) <* manyTill C1.anyToken eof

unsat :: Parser String
unsat = (many comment) >> string "=====UNSATISFIABLE====="

solutions :: Parser [Solution]
solutions = manyTill solution (string "==========")

solution :: Parser Solution
solution = fmap clearSolution maybeSolution <* (string "----------" >> endOfLine)

maybeSolution :: Parser [(Maybe (String, MValue))]
maybeSolution = many $ (comment >> return Nothing) <|> (Just <$> (assigned >>= return))

clearSolution :: [(Maybe a)] -> [a]
clearSolution [] = []
clearSolution (Nothing:ls) = clearSolution ls
clearSolution ((Just v):ls) = v : clearSolution ls

comment :: Parser String
comment = char '%' *> (manyTill C1.anyToken endOfLine) *> return ""

assigned :: Parser (String, MValue)
assigned = do
  name <- varName
  value <- valueParser
  char ';'
  endOfLine
  return (name, value)

varName :: Parser String
varName = manyTill (C.noneOf "-=%") ((C.space >> char '=' >> C.space) <|> char '=')

valueParser :: Parser MValue
valueParser = try floatM <|> intM <|> boolM <|> (setM scalar) <|> (array scalar) <|> stringM

intM :: Parser MValue
intM = MInt <$> int

boolM :: Parser MValue
boolM = MBool <$> bool

floatM :: Parser MValue
floatM = MFloat <$> float

stringM :: Parser MValue
stringM = MString <$> (many anyChar)

setM :: Parser MValue -> Parser MValue
setM p = (MSet <$> S.fromDistinctAscList <$> (set p)) <|> setRange

int :: Parser Int
int = (char '-' >> opposite ) <|> natural

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

setRange :: Parser MValue
setRange = MSet <$> S.fromDistinctAscList <$> do
  v1 <- int
  string ".."
  v2 <- int
  return (map MInt (take (v2 - v1 + 1) (iterate ((+) 1) v1)))

array :: Parser MValue -> Parser MValue
array p = do
  string "array"
  manyTill anyChar (char '(')
  ls <- arraySizes 
  es <- extract p
  string ")"
  return (fixDims ls es)

natural :: Parser Int
natural = C1.chainl1 digitValue ascendDecimal

opposite :: Parser Int
opposite = (0 - ) <$> natural

digitValue :: Parser Int
digitValue = do
  d <- digit
  return $ ord(d) - ord('0')

ascendDecimal :: Parser (Int -> Int -> Int)
ascendDecimal = do
  return $ \x y -> x*10 + y  

indexRange :: Parser Int
indexRange = do
  a <- int
  string ".."
  b <- int
  return (b - a + 1)
  
arraySizes :: Parser [Int]
arraySizes = C1.sepEndBy1 indexRange (string ",")

extract :: Parser MValue -> Parser [MValue]
extract p = between (char '[') (char ']') (sepBy p (string ","))

fixDims :: [Int] -> [MValue] -> MValue
fixDims [] _ = MError "Array dimensions error: fixDims applied on empty list"
fixDims [d] ms = MArray $ ms
fixDims ds ms = fixDims (init ds) (fix1Dim (last ds) ms)

fix1Dim :: Int -> [MValue] -> [MValue]
fix1Dim _ [] = []
fix1Dim d ms = MArray (take d ms) : (fix1Dim d (drop d ms))

scalar :: Parser MValue
scalar = try floatM <|> intM <|> boolM <|> stringM

-- for testing purposes
parseWithLeftOver :: Parser a -> String -> Either P.ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill C1.anyToken C1.eof
