{-|
Module      : MZPrinter
Description : MiniZinc pretty-printer
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module provides a pretty-printer of MiniZinc models represented through the "MZAST" module.
This pretty-printer is based on the "Text.PrettyPrint" module.
-}

module Interfaces.MZPrinter(
  Interfaces.MZAST.MZModel,
  printModel,
  printItem,
  printExpr
) where

import Text.PrettyPrint
import Data.List
import Interfaces.MZAST
  
-- | Prints the represented MiniZinc model. Essentially, this function applies 'printItem' on
-- each element of the specified model.
printModel :: MZModel -> Doc
printModel = foldl1 ($+$) . map printItem

-- | Prints an item of the represented model. Example:
-- 
-- >>> printItem $ Pred "even" [(Dec, Int, "x")] (Just (Bi Eq (Bi Mod (Var "x") (IConst 2)) (IConst 0)))
-- predicate even(var int: x) =
--   x mod 2 = 0;
printItem :: Item -> Doc
printItem (Empty)                   = text ""
printItem (Comment str)             = text "%" <+> text str
printItem (Include file)            = text "include" <+> doubleQuotes (text file) <> semi
printItem (Declare (_, vt@(Array _ _)) name me) = case me of
                                                    Nothing -> (printVarType vt) <> colon <+> text name <> semi
                                                    Just e -> (printVarType vt) <> colon <+> text name <+> equals <+> printExpr e <> semi
printItem (Declare (ins, vt) name me)           = case me of
                                                    Nothing -> printTypeInst (ins, vt) <> colon <+> text name <> semi
                                                    Just e -> printTypeInst (ins, vt) <> colon <+> text name <+> equals <+> printExpr e <> semi
printItem (Constraint c)            = text "constraint" <+> printExpr c <> semi
printItem (Assign var expr)         = text var <+> equals <+> printExpr expr <> semi
printItem (Output e)                = text "output" <+> printExpr e <> semi
printItem (Solve s)                 = text "solve" <+> printSolve s <> semi
printItem (Pred name ps me)         = case me of 
                                        Nothing -> text "predicate" <+> text name <> parens (commaSep printParam ps) <> semi
                                        Just e  -> text "predicate" <+> text name <> parens (commaSep printParam ps) <+> equals $+$ nest 2 (printExpr e) <> semi
printItem (Test name ps me)         = case me of 
                                        Nothing -> text "test" <+> text name <> parens (commaSep printParam ps) <> semi
                                        Just e  -> text "test" <+> text name <> parens (commaSep printParam ps) <+> equals $+$ nest 2 (printExpr e) <> semi
printItem (Function ti name ps me)  = case me of 
                                        Nothing -> text "function" <+> printTypeInst ti <> colon <+> text name <> parens (commaSep printParam ps) <> semi
                                        Just e  -> text "function" <+> printTypeInst ti <> colon <+> text name <> parens (commaSep printParam ps) <+> equals $+$ nest 2 (printExpr e) <> semi

-- | Prints the represented MiniZinc expressions of a model. Examples:
-- 
-- >>> printExpr $ SetComp (Bi Times (IConst 2) (Var "i")) ([(["i"], Interval (IConst 1) (IConst 5))], Nothing)
-- {2 * i | i in 1..5}
-- 
-- >>> printExpr $ Let [Declare Dec Int "x" (Just (IConst 3)), Declare Dec Int "y" (Just (IConst 4))] (Bi BPlus (Var "x") (Var "y"))
-- let {var int: x = 3;
--      var int: y = 4;}
-- in x + y
printExpr :: Expr -> Doc
printExpr AnonVar             = text "_"
printExpr (Var v)             = text v
printExpr (BConst b)
  | b                         = text "true"
  | otherwise                 = text "false"
printExpr (IConst n)          = int n
printExpr (FConst x)          = float x
printExpr (SConst str)        = doubleQuotes $ text (escape str)
printExpr (Interval e1 e2)    = printParensExpr 0 e1 <> text ".." <> (printParensExpr 0 e2)
printExpr (SetLit es)         = braces $ commaSepExpr es
printExpr (SetComp e ct)      = braces (printExpr e <+> text "|" <+> printCompTail ct)
printExpr (ArrayLit es)       = brackets $ commaSepExpr es
printExpr (ArrayLit2D ess)    = brackets (foldl1 ($+$) (map (\x -> text "|" <+> commaSepExpr x) ess) <> text "|")
printExpr (ArrayComp e ct)    = brackets (printExpr e <+> text "|" <+> printCompTail ct)
printExpr (ArrayElem v es)    = text v <> brackets (commaSepExpr es)
printExpr (U op e)            = printUop op <+> (if isAtomic e then printExpr e else parens (printExpr e))
printExpr (Bi op e1 e2)       = printParensExpr (prec op) e1 <+> printBop op <+> printParensExpr (prec op) e2
printExpr (Call f es)         = printFunc f <> parens (commaSepExpr es)
printExpr (ITE [(e1, e2)] e3) = text "if" <+> printExpr e1 <+> text "then" <+> printExpr e2 
                                $+$ text "else" <+> printExpr e3 <+> text "endif"
printExpr (ITE (te:tes) d)    = text "if" <+> printExpr (fst te) <+> text "then" <+> printExpr (snd te) 
                                $+$ printEITExpr tes 
                                $+$ text "else" <+> printExpr d <+> text "endif"
printExpr (Let is e)          = text "let" <+> braces (nest 4 (vcat (map printItem is))) $+$ text "in" <+> printExpr e
printExpr (GenCall f ct e)    = printFunc f <> parens (printCompTail ct)
                                $+$ nest 2 (parens (printExpr e))

-- Only helps for printing if-then-elseif-then-...-else-endif expressions
printEITExpr :: [(Expr,Expr)] -> Doc
printEITExpr [] = empty
printEITExpr (te:tes) = text "elseif" <+> printExpr (fst te) <+> text "then" <+> printExpr (snd te) $+$ printEITExpr tes
-- This function together with prec are used for placing parentheses in expressions
printParensExpr :: Int -> Expr -> Doc
printParensExpr n e@(Bi op _ _)
  | n < prec op = parens (printExpr e)
  | otherwise   = printExpr e
printParensExpr _ e@(U _ ue) = if isAtomic ue then printExpr ue else parens (printExpr ue)
printParensExpr _ e = printExpr e

prec :: Bop -> Int
prec LRarrow  = 7 
prec Rarrow   = 7
prec Larrow   = 7
prec And      = 7 
prec Or       = 7
prec Eqq      = 8
prec Neq      = 8
prec Times    = 9
prec Mod      = 9
prec _        = 10

-- Prints the instantiation (var or par) and the type in a variable declaration
printTypeInst :: TypeInst -> Doc
printTypeInst (i, t) = printInst i <+> printVarType t


printVarType :: VarType -> Doc
printVarType Bool           = text "bool"
printVarType Float          = text "float"
printVarType Int            = text "int"
printVarType String         = text "string"
printVarType (Set t)        = text "set of" <+> printVarType t
printVarType (Array ts ti)  = text "array" <> brackets (commaSep printVarType ts) <+> text "of" <+> printTypeInst ti
printVarType (List ti)      = text "list of" <+> printTypeInst ti
printVarType (Opt t)        = text "opt" <+> printVarType t
printVarType (Range e1 e2)  = printExpr e1 <> text ".." <> printExpr e2
printVarType (Elems es)     = braces $ commaSepExpr es
printVarType Any            = text "any"
printVarType (AOS name)     = text name

printCompTail :: CompTail -> Doc
printCompTail (gs, Nothing) = commaSep printGenerator gs
printCompTail (gs, Just wh) = commaSep printGenerator gs <+> text "where" <+> printExpr wh

printGenerator :: Generator -> Doc
printGenerator (es, r) = text (intercalate ", " es) <+> text "in" <+> printExpr r

printInst :: Inst -> Doc
printInst Dec = text "var"
printInst Par = text "par"

printFunc :: Func -> Doc
printFunc (CName name) = text name
printFunc (PrefBop op) = text "`" <> printBop op <> text "`"

printBop :: Bop -> Doc
printBop Gt           = text ">"
printBop Lt           = text "<"
printBop Lte          = text "<="
printBop Gte          = text ">="
printBop Eq           = equals
printBop Eqq          = equals <> equals
printBop Neq          = text "!="
printBop BPlus        = text "+"
printBop BMinus       = text "-"
printBop Times        = text "*"
printBop Div          = text "/"
printBop IDiv         = text "div"
printBop Mod          = text "mod"
printBop LRarrow      = text "<->"
printBop Larrow       = text "<-"
printBop Rarrow       = text "->"
printBop And          = text "/\\"
printBop Or           = text "\\/"
printBop In           = text "in"
printBop Sub          = text "subset"
printBop Super        = text "superset"
printBop Union        = text "union"
printBop Inters       = text "intersect"
printBop Diff         = text "diff"
printBop SDiff        = text "symdiff"
printBop RangeOp      = text ".."
printBop Concat       = text "++"
printBop (AsFunc op)  = quotes $ printBop op

printUop :: Uop -> Doc
printUop Not    = text "not"
printUop UPlus  = text "+"
printUop UMinus = text "-"

printSolve :: Solve -> Doc
printSolve Satisfy      = text "satisfy"
printSolve (Minimize e) = text "minimize" <+> printExpr e
printSolve (Maximize e) = text "maximize" <+> printExpr e

-- Prints the parameters of user defined call expressions (predicates, tests and functions)
printParam :: Param -> Doc
printParam (i, t, n) = printInst i <+> printVarType t <> colon <+> text n

-- Horizontally concatinates Docs while also putting a comma-space (", ") in between
commaSepDoc :: [Doc] -> Doc
commaSepDoc = hsep . punctuate comma

-- Vertically prints expressions, one per line
lineSepExpr :: [Expr] -> Doc
lineSepExpr = vcat . map printExpr

-- First, map a function to a list and produce a list of Docs and then apply commaSepDoc
commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f ls = commaSepDoc $ map f ls

-- Special case of commaSep, where f = printExpr
commaSepExpr :: [Expr] -> Doc
commaSepExpr = commaSep printExpr

isAtomic :: Expr -> Bool
isAtomic AnonVar = True
isAtomic (Var _)      = True
isAtomic (BConst _)   = True
isAtomic (IConst _)   = True
isAtomic (FConst _)   = True
isAtomic (SConst _)   = True
isAtomic (SetLit _)   = True
isAtomic _            = False

escape:: String -> String
escape str = concatMap escapeChar str

escapeChar :: Char -> String
escapeChar '\n' = "\\n"
escapeChar '\t' = "\\t"
escapeChar '\r' = "\\r"
escapeChar '\\' = "\\\\"
escapeChar '\f' = "\\f"
escapeChar '\a' = "\\a"
escapeChar c = [c]