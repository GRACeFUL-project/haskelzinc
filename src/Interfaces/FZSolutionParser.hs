{-# LANGUAGE FlexibleInstances#-}

{-|
Module      : FZSolutionParser
Description : FlatZinc solutions parser
License     : GPL-3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module defines a parser for the default format of the output of the two solvers 
integrated in haskelzinc (G12/FD and choco3). It also provides modular parsers for 
entities that constitute a solution, such as MiniZinc variable names and values, 
solutions' separator in case of multiple solutions, etc. These modular parsers can be 
used in building a parser for a solver's output, the format of which is specified by a 
MiniZinc @output@ item differs from the default one.
-}

module Interfaces.FZSolutionParser (
  MValue(..), Solution,
  -- * Parsing values
  valueM,
  intM, boolM, floatM, stringM, setM,
  setRange, arrayM,
  -- * Solutions
  varName, simpleVarName, quotedVarName,
  comment, comments,
  -- ** Default parsers
  defaultNameValuePair,
  defaultUnsat, defaultSolution, defaultSolutions,
  tryDefaultSolutions,
  getDefaultSolutions, getDefaultSolutionsFromFile,
  -- ** Custom
  -- | The following functions can be used when a MiniZinc @output@ item, which alters the 
  -- default output format of the solver, is present in the model.
  
  nameValuePair,
  trySolutions, getSolutions
) where

import Data.Char
import Control.Applicative
import Data.Set (Set, fromDistinctAscList)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec.String (Parser)
-- Next two modules for testing only
--import GHC.Generics
--import Control.DeepSeq

-- | A Solution consists of a list of pairs. Each pair represents an assignment of a 
-- value to a decision variable of the constraint model.
type Solution = [(String, MValue)]

-- | Representation of returned values.
data MValue = MError String
            | MInt Int
            | MFloat Float
            | MBool Bool
            | MString String
            | MArray [MValue]
            | MSet (Set MValue)
  deriving Show
  --deriving (Show, Generic, NFData)

-- | Returns either a parse error or a list of solutions of the constraint model, parsed 
-- from the file where they are printed. The length of the list is specified by the 
-- second argument of the function.
getDefaultSolutionsFromFile :: FilePath -> Int -> IO (Either P.ParseError [Solution])
getDefaultSolutionsFromFile path n = do
  output <- readFile path
  return $ runParser (tryDefaultSolutions n) output

-- | Same as 'getSolutionFromFile' but parses the string argument of the function instead
-- of the contents of a file.
getDefaultSolutions :: Int -> String -> Either P.ParseError [Solution]
getDefaultSolutions = getSolutions tryDefaultSolutions

-- | A custom version of 'getDefaultSolutions'. This function accepts a custom parser to 
-- parse the solutions. The custom parser must be parametrized by an integer, for 
-- specifying the number of solutions to be returned.
getSolutions :: (Int -> Parser [Solution]) -> Int -> String -> Either P.ParseError [Solution]
getSolutions p n = runParser (p n)

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

-- Defaults
unsatMSG = "=====UNSATISFIABLE====="  -- Unsatisfiable-model message
eoSMSG = "=========="                 -- End-of-solutions message
eosMSG = "----------"                 -- End-of-solution message
-----------------------

runParser :: Parser a -> String -> Either P.ParseError a
runParser p = parse (p <* eof) ""

-- | @tryDefaultSolutions n@ tries to parse the solutions and, if it succeeds, returns 
-- the first @n@. Else, tries 'defaultUnsat' and returns an empty list.
tryDefaultSolutions :: Int -> Parser [Solution]
tryDefaultSolutions = trySolutions takeSolutions defaultUnsat

-- | @trySolutions f p n@ applies @f n@ and returns the solutions. If that fails, tries 
-- to parse an /Unsatisfiable/ message by applying @p@ and returns an empty list. The 
-- custom parser must be parametrized by an integer, for specifying the number of 
-- solutions to be returned.
trySolutions :: (Int -> Parser [Solution]) -- Custom solutions parser
             -> Parser String              -- Custom /Unsatisfiable/ message parser
             -> Int                        -- Number of solutions to be returned
             -> Parser [Solution]
trySolutions p u n = try $ (p n) <|> (u >> return [[]])

-- | Parses the default message for a model with no solutions: @=====UNSATISFIABLE=====@, 
-- surrounded by commented lines before and after.
defaultUnsat :: Parser String
defaultUnsat = skipMany comment *> (string unsatMSG) <* endOfLine <* many comment

takeSolutions :: Int -> Parser [Solution]
takeSolutions n = take n <$> defaultSolutions

-- | Parses all the returned solutions.
defaultSolutions :: Parser [Solution]
defaultSolutions = manyTill defaultSolution (string eoSMSG *> endOfLine)

-- | Parses a single solution with the default output format from the set of returned 
-- solutions.
defaultSolution :: Parser Solution
defaultSolution =  P.many (comments *> defaultNameValuePair)
                   <* string eosMSG <* endOfLine

-- | Parses a comment in the solutions and returns the content.
comment :: Parser String
comment = char '%' *> spaces *> (manyTill anyToken endOfLine)

-- | Parses a sequence of commented lines in the solutions and returns their content.
comments :: Parser String
comments = unlines <$> P.many comment

-- | Parses a MiniZinc variable name-value pair in a solution with the default output
-- format.
defaultNameValuePair :: Parser (String, MValue)
defaultNameValuePair =  nameValuePair (spaces *> (string "=") <* spaces) 
                     <* ((: []) <$> (char ';' *> endOfLine))

-- | Used to parse a MiniZinc variable name-value pair in a solution.
-- @nameValuePair s@ parses succesfully if sequential parsing of 'varName', @s@ and 
-- 'valueM' is succesfull. Returns the MiniZinc name-value pair in a Haskell pair and 
-- /forgets/ the result of parser @s@.
nameValuePair :: Parser String -- ^ Value-name separator
              -> Parser (String, MValue)
nameValuePair p1 = do
  name <- varName
  p1
  value <- valueM
  return (name, value)

-- | Parses a conventional MiniZinc variable identifier. That is, a string of the form 
-- @[A-Za-z][A-Za-z0-9_]*@.
simpleVarName :: Parser String
simpleVarName = do
  first <- C.letter
  rest <- P.many (C.alphaNum <|> char '_')
  return (first : rest)

-- | Parses a quoted MiniZinc identifier.
quotedVarName :: Parser String
quotedVarName = do
  lq <- char '\''
  name <- manyTill anyChar (char '\'')
  return (lq : (name ++ "\'"))

-- | Parses a MiniZinc variable name by trying 'simpleVarName' and 'quotedVarName'.
varName :: Parser String
varName = simpleVarName <|> quotedVarName

-- | Parses a MiniZinc value. Tries 'floatM', 'intM', 'boolM', 'setM', 'arrayM' and
-- 'stringM' in this order.
valueM :: Parser MValue
valueM = try floatM <|> intM <|> boolM <|> (setM scalar) <|> (arrayM scalar) <|> stringM

-- | Parses a MiniZinc integer value.
intM :: Parser MValue
intM = MInt <$> int

-- | Parses a MiniZinc boolean value.
boolM :: Parser MValue
boolM = MBool <$> bool

-- | Parses a MiniZinc float value.
floatM :: Parser MValue
floatM = MFloat <$> float

-- | Parses a MiniZinc string value.
stringM :: Parser MValue
stringM = MString <$> (string "\"" *> manyTill anyChar (string "\""))

-- | Parses a MiniZinc set value.
setM :: Parser MValue -> Parser MValue
setM p = (MSet <$> fromDistinctAscList <$> (set p)) <|> setRange

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

-- | Parses a MiniZinc set value defined with the use of the MiniZinc range operator 
-- (@..@).
setRange :: Parser MValue
setRange = MSet <$> fromDistinctAscList <$> do
  v1 <- int
  string ".."
  v2 <- int
  return (map MInt (take (v2 - v1 + 1) (iterate ((+) 1) v1)))

-- | Parses MiniZinc 1-dimensional or multi-dimensional array values.
arrayM :: Parser MValue -> Parser MValue
arrayM p = do
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
