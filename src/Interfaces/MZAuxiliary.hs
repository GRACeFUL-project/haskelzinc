{-|
Module      : MZAuxiliary
Description : Contains auxiliary definitions
License     : GPL-3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module handles the configuration needed to run constraint models.
-}

module Interfaces.MZAuxiliary(
  Configuration(..),
  parseConfig,
  spaceFix
) where


import System.Process
import System.FilePath
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as C1

-- | A record containing the required paths for the constraint models to be run by
-- the solvers.
data Configuration 
  = Config { minizinc    :: FilePath -- ^ Path to the directory of @mzn2fzn@ and 
                                     -- @flatzinc@ executables
           , chocosolver :: FilePath -- ^ Path to the choco_solver java library
           , chocoparser :: FilePath -- ^ Path to the choco_parser java library
           , antlr_path  :: FilePath -- ^ Path to the antlr java library
           }
  deriving Show

instance Monoid Configuration where
  mempty = Config { minizinc = ""
                  , chocosolver = ""
                  , chocoparser = ""
                  , antlr_path = ""
                  }
   
  mappend a b = 
    Config { minizinc    = dropEmpty (minizinc a) (minizinc b)
           , chocosolver = dropEmpty (chocosolver a) (chocosolver b)
           , chocoparser = dropEmpty (chocoparser a) (chocoparser b)
           , antlr_path  = dropEmpty (antlr_path a) (antlr_path b)
           }

dropEmpty :: String -> String -> String
dropEmpty "" "" = ""
dropEmpty a ""  = a
dropEmpty "" b  = b

makeConf :: Either (P.ParseError) (String, String) -> Configuration
makeConf (Right (name, path))
  | name == conf_mz = Config { minizinc    = path
                             , chocosolver = ""
                             , chocoparser = ""
                             , antlr_path  = ""
                             }
  | name == conf_cs = Config { minizinc    = ""
                             , chocosolver = path
                             , chocoparser = ""
                             , antlr_path  = ""
                             }
  | name == conf_cp = Config { minizinc    = ""
                             , chocosolver = ""
                             , chocoparser = path
                             , antlr_path  = ""
                             }
  | name == conf_an = Config { minizinc    = ""
                             , chocosolver = ""
                             , chocoparser = ""
                             , antlr_path  = path
                             }
makeConf (Right (_,_)) = mempty
makeConf (Left err) = mempty

-- Definitions

choco = "CHOCO_"
conf_mz = "MINIZINC_DIR"
conf_cs = "SOLVER"
conf_cp = "PARSER"
conf_an = "ANTLR"

parser_choco = string choco
parser_mz = string conf_mz
parser_cs = parser_choco >> string "SOLVER"
parser_cp = parser_choco >> string "PARSER"
parser_an = string conf_an

emptyConf = Config { minizinc    = ""
                   , chocosolver = ""
                   , chocoparser = ""
                   , antlr_path  = ""
                   }
                
confFile = joinPath ["HZconf", "conf.txt"]

-- | Parses the configuration file.
parseConfig = do
  contents <- readFile confFile
  return $ configure (lines contents)

parserLine :: Parser (String, String)
parserLine = do
  left <- try parser_mz <|> (try parser_cs <|> parser_cp) <|> parser_an
  C.spaces
  char '='
  C.spaces
  right <- parserr
  return (left,right)
  
parserr :: Parser String
parserr = manyTill anyChar eof

configure ls = mconcat (map (makeConf . (runParser parserLine)) ls)

-- | Wraps a path in quotes if it contains spaces.
spaceFix :: String -> String
spaceFix str = if elem ' ' str
               then "\"" ++ str ++ "\""
               else str

runParser :: Parser a -> String -> Either P.ParseError a
runParser p = P.parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either P.ParseError (a,String)
parseWithLeftOver p = P.parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof
  
-- Needed definitions
manyTill = C1.manyTill
try = P.try
anyToken = C1.anyToken
anyChar :: Parser Char
anyChar = C.anyChar
endOfLine :: Parser Char
endOfLine = C.endOfLine
char = C.char
eof = C1.eof
(<|>) = (P.<|>)
string :: String -> Parser String
string = C.string