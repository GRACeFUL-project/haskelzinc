{-
  There is some configuration needed before one can run
  a model. This script works for Windows. See comments below.
-}
{-# LANGUAGE CPP #-}

module Interfaces.Auxiliary(
  Configuration(..),
  parseConfig,
  spaceFix,
  makePath
) where


import System.Process
import System.FilePath.Posix
import Data.List
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as C1

data Configuration
  = Config { minizinc    :: FilePath
           , chocosolver :: FilePath
           , chocoparser :: FilePath
           , antlr_path  :: FilePath
           }
  deriving Show

instance Monoid Configuration where
  mempty = Config { minizinc    = ""
                  , chocosolver = ""
                  , chocoparser = ""
                  , antlr_path  = ""
                  }

  mappend a b =
    Config { minizinc    = dropEmpty (minizinc a)    (minizinc b)
           , chocosolver = dropEmpty (chocosolver a) (chocosolver b)
           , chocoparser = dropEmpty (chocoparser a) (chocoparser b)
           , antlr_path  = dropEmpty (antlr_path a)  (antlr_path b)
           }

dropEmpty :: String -> String -> String
dropEmpty "" "" = ""
dropEmpty a ""  = a
dropEmpty "" b  = b
dropEmpty a  b  = error $ "dropEmpty: neither arg. was empty: " ++ a ++ " and " ++ b

makeConf :: Either P.ParseError (String, String) -> Configuration
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
makeConf (Left err)    = mempty

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

confFile = makePath ["HZconf", "conf.txt"]
-- /Definitions

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

--tryAll = try $ (parserLine conf_mz) <|> (try ((parserLine conf_cs) <|> (parserLine conf_cp))) <|> (parserLine conf_an)

configure ls = mconcat (map (makeConf . runParser parserLine) ls)

-- Function only needed for filepaths in Windows
spaceFix :: String -> String
#ifdef unix
spaceFix = id
#else
spaceFix str = if ' '  `elem`  str
               then "\"" ++ str ++ "\" "
               else str
#endif

makePath :: [String] -> String
makePath = intercalate [pathSeparator]

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
