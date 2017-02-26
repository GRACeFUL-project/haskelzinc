{-# LANGUAGE FlexibleInstances#-}

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
format of the solver's output.
-}

module Interfaces.FZSolutionParser (
  getSolution,
  printSolution,
  getSolutionFromFile,
  printSolutionFromFile,
  Solution,
  MValue(..)
) where

import Data.Char
import Control.Applicative
import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec.String (Parser)
-- Next two modules for testing only
--import GHC.Generics
--import Control.DeepSeq

-- | A Solution consists of a list of pairs. Each pair represents an assignment of a value to
-- a decision variable of the constraint model.
type Solution = [(String, MValue)]

-- | Representation of returned values.
data MValue = MError String
            | MInt Int
            | MFloat Float
            | MBool Bool
            | MString String
            | MArray [MValue]
            | MSet (S.Set MValue)
  deriving Show
  --deriving (Show, Generic, NFData)

-- | Returns either a parse error or a list of solutions of the constraint model, parsed from 
-- the file where they are printed. The length of the list is specified by the second argument 
-- of the function.
getSolutionFromFile :: FilePath -> Int -> IO (Either P.ParseError [Solution])
getSolutionFromFile path n = do
  output <- readFile path
  return $ runParser (trySolutions n) output

-- | Prints either a parse error or a list of solutions of the constraint model parsed from 
-- a file where they are printed. The length of the list is sepcified by the second argument 
-- of the function.
printSolutionFromFile :: Int -> FilePath -> IO ()
printSolutionFromFile n path = do
  output <- readFile path
  print $ runParser (trySolutions n) output

-- | Same as @getSolutionFromFile@ but parses the string argument of the function instead
-- of the contents of a file.
getSolution :: Int -> String -> Either P.ParseError [Solution]
getSolution n = runParser (trySolutions n)

-- | Same as @printSolutionFromFile@ but parses the string argument of the function instead
-- of the contents of a file.
printSolution :: Int -> String -> IO ()
printSolution n = print . runParser (trySolutions n)

-- Auxiliary definitions
digit :: Parser Char
digit = C.digit
  
anyChar :: Parser Char
anyChar = C.anyChar

char :: Char -> Parser Char
char = C.char

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy = P.sepBy

between :: Parser a -> Parser b -> Parser c -> Parser c
between = P.between

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill = P.manyTill

many1 :: Parser a -> Parser [a]
many1 = P.many1

skipMany :: Parser a -> Parser ()
skipMany = P.skipMany

anyToken = P.anyToken

eof :: Parser ()
eof = P.eof

endOfLine :: Parser Char
endOfLine = C.endOfLine

string :: String -> Parser String
string = C.string

spaces :: Parser ()
spaces = C.spaces

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse

try :: Parser a -> Parser a
try = P.try
-----------------------

runParser :: Parser a -> String -> Either P.ParseError a
runParser p = parse (p <* eof) ""

trySolutions :: Int -> Parser [Solution]
trySolutions n = (try (takeSolutions n) <|> (unsat >> return [[]]))

unsat :: Parser String
unsat = skipMany comment *> (string "=====UNSATISFIABLE=====") <* endOfLine <* many comment

takeSolutions :: Int -> Parser [Solution]
takeSolutions n = take n <$> (solutions)

solutions :: Parser [Solution]
solutions = manyTill solution (string "==========" >> endOfLine)

solution :: Parser [(String, MValue)]
solution = (many $ (skipMany comment) *> (assigned >>= return)) 
                   <* string "----------" <* endOfLine

comment :: Parser String
comment = char '%' *> (manyTill anyToken endOfLine) *> return ""

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
set p = between (char '{') (char '}') (sepBy p (string "," >> spaces))

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
natural = P.chainl1 digitValue ascendDecimal

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
arraySizes = P.sepEndBy1 indexRange (string "," >> spaces)

extract :: Parser MValue -> Parser [MValue]
extract p = between (char '[') (char ']') (sepBy p (string "," >> spaces))

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
  where leftOver = manyTill anyToken eof
