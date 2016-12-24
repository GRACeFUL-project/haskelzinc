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
  printNakedExpr,
  printExpr,
  layout
) where

import Text.PrettyPrint
import Data.List
import Interfaces.MZAST
import Interfaces.MZBuiltIns

layout :: MZModel -> String
layout = render . printModel
  
-- | Prints the represented MiniZinc model. Essentially, this function applies 'printItem' on
-- each element of the specified model.
printModel :: MZModel -> Doc
printModel = foldr1 ($+$) . map printItem -- foldR1?

-- | Prints an item of the represented model. Example:
-- 
-- >>> printItem $ Pred "even" [(Dec, Int, "x")] (Just (Bi Eq (Bi Mod (Var "x") (IConst 2)) (IConst 0)))
-- predicate even(var int: x) =
--   x mod 2 = 0;
printItem :: Item -> Doc
printItem (Empty)                 = text ""
printItem (Comment str)           = text "%" <+> text str
printItem (Include file)          = text "include" <+> doubleQuotes (text file) <> semi
printItem (Declare p ans me)      = printCall (printParam p) me
                                    <> semi
printItem (Constraint c)          = hang (text "constraint") 2 (printExpr c <> semi)
printItem (Assign var expr)       = printCall (text var) (Just expr)
                                    <> semi
printItem (Output e)              = text "output" 
                                    <+> printNakedExpr e 
                                    <> semi
printItem (AnnotDec name ps)      = text "annotation" 
                                    <+> text name 
                                    <> parens (printParams ps) 
                                    <> semi
printItem (Solve ans s)           = text "solve" 
                                    <+> printAnnotations ans
                                    <+> printSolve s 
                                    <> semi
printItem (Pred name ps ans me)   = printCall (text "predicate" 
                                    <+> text name 
                                    <> parens (printParams ps)
                                    <+> printAnnotations ans
                                    ) me <> semi
printItem (Test name ps ans me)   = printCall (text "test" 
                                    <+> text name 
                                    <> parens (printParams ps)
                                    <+> printAnnotations ans
                                    ) me <> semi
printItem (Function p ps ans me)  = printCall (text "function" 
                                    <+> printParam p 
                                    <> parens (printParams ps) 
                                    <+> printAnnotations ans
                                    ) me <> semi

printCallName :: String -> Maybe Param -> Ident -> [Annotation] -> [Param] -> Doc
printCallName s Nothing  name ans ps = text s
                                       <> parens (printParams ps)
                                       <+> printAnnotations ans
printCallName s (Just p) name ans ps = text s
                                       <+> printParam p
                                       <> parens (printParams ps)
                                       <+> printAnnotations ans

printCallBody :: Maybe NakedExpr -> Doc
printCallBody v = maybe empty printNakedExpr v

printCall :: Doc -> Maybe NakedExpr -> Doc
printCall d v  = hang (d <+> equals) 2 (printCallBody v)

printExpr :: Expr -> Doc
printExpr (Expr e ans) = printNakedExpr e <> printAnnotations ans

-- | Prints the represented MiniZinc expressions of a model. Examples:
-- 
-- >>> printNakedExpr $ SetComp (Bi Times (IConst 2) (Var "i")) ([(["i"], Range (IConst 1) (IConst 5))], Nothing)
-- {2 * i | i in 1..5}
-- 
-- >>> printNakedExpr $ Let [Declare Dec Int "x" (Just (IConst 3)), Declare Dec Int "y" (Just (IConst 4))] (Bi BPlus (Var "x") (Var "y"))
-- let {var int: x = 3;
--      var int: y = 4;}
-- in x + y
printNakedExpr :: NakedExpr -> Doc
printNakedExpr AnonVar             = text "_"
printNakedExpr (Var v)             = text v
printNakedExpr (BConst b)
  | b                              = text "true"
  | otherwise                      = text "false"
printNakedExpr (IConst n)          = int n
printNakedExpr (FConst x)          = float x
printNakedExpr (SConst str)        = doubleQuotes $ text (escape str)
printNakedExpr (SetLit es)         = braces $ commaSepExpr es
printNakedExpr (SetComp e ct)      = braces ( printNakedExpr e 
                                              <+> text "|" 
                                              <+> printCompTail ct )
printNakedExpr (ArrayLit es)       = brackets $ commaSepExpr es
printNakedExpr (ArrayLit2D ess)    = 
  brackets (foldl1 ($+$) (map (\x -> text "|" <+> commaSepExpr x) ess) <> text "|")
printNakedExpr (ArrayComp e ct)    = brackets (printNakedExpr e <+> text "|" <+> printCompTail ct)
printNakedExpr (ArrayElem v es)    = text v <> brackets (commaSepExpr es)
printNakedExpr (U op e)            = 
  printOp op <+> (if isAtomic e then printNakedExpr e else parens (printNakedExpr e))
printNakedExpr (Bi op e1 e2)       = sep [printParensNakedExpr (opPrec op) e1 
                                         , printOp op 
                                         , printParensNakedExpr (opPrec op) e2]
printNakedExpr (Call f)            = printCallable f
printNakedExpr (ITE [(e1, e2)] e3) = text "if" <+> printNakedExpr e1 
                                     <+> text "then" <+> printNakedExpr e2 
                                     $+$ text "else" <+> printNakedExpr e3 <+> text "endif"
printNakedExpr (ITE (te:tes) d)    = text "if" <+> printNakedExpr (fst te) 
                                     <+> text "then" <+> printNakedExpr (snd te) 
                                     $+$ printEITExpr tes 
                                     $+$ text "else" <+> printNakedExpr d <+> text "endif"
printNakedExpr (Let is e)          = text "let" 
                                     <+> braces (nest 4 (vcat (map printItem is))) 
                                     $+$ text "in" 
                                     <+> printNakedExpr e
printNakedExpr (GenCall name ct e) = text name <> parens (printCompTail ct)
                                     $+$ nest 2 (parens (printNakedExpr e))

-- Only helps for printing if-then-elseif-then-...-else-endif expressions
printEITExpr :: [(NakedExpr, NakedExpr)] -> Doc
printEITExpr [] = empty
printEITExpr (te:tes) = text "elseif" 
                        <+> printNakedExpr (fst te) 
                        <+> text "then" 
                        <+> printNakedExpr (snd te) 
                        $+$ printEITExpr tes

-- This function together with prec are used for placing parentheses in expressions
printParensNakedExpr :: Int -> NakedExpr -> Doc
printParensNakedExpr n e@(Bi op _ _)
  | n < opPrec op  = parens (printNakedExpr e)
  | otherwise    = printNakedExpr e
printParensNakedExpr _ e@(U _ ue) = if isAtomic ue 
                                    then printNakedExpr ue 
                                    else parens (printNakedExpr ue)
printParensNakedExpr _ e          = printNakedExpr e

printType :: Type -> Doc
printType Bool           = text "bool"
printType Float          = text "float"
printType Int            = text "int"
printType String         = text "string"
printType (Set t)        = text "set of" <+> printType t
printType (Array ts ti)  = text "array" <> brackets (commaSep printType ts) 
                           <+> text "of" <+> printTypeInst ti
printType (List ti)      = text "list of" <+> printTypeInst ti
printType (Opt t)        = text "opt" <+> printType t
printType (Ann)          = text "ann"
printType (Range e1 e2)  = printParensNakedExpr 0 e1 
                           <> text ".." 
                           <> printParensNakedExpr 0 e2
printType (Elems es)     = braces $ commaSepExpr es
printType (AOS name)     = text name
printType (VarType name) = text "$" <> text name

printCompTail :: CompTail -> Doc
printCompTail (gs, Nothing) = commaSep printGenerator gs
printCompTail (gs, Just wh) = commaSep printGenerator gs <+> text "where" <+> printNakedExpr wh

printGenerator :: Generator -> Doc
printGenerator (es, r) = text (intercalate ", " es) <+> text "in" <+> printNakedExpr r

printInst :: Inst -> Doc
printInst Dec = text "var"
printInst Par = text "par"

printCallable :: Callable -> Doc
printCallable (Callable name args) = 
  text name 
  <> cat (putParens $ punctuateBefore comma (map printNakedExpr args))

printAnnotations :: [Annotation] -> Doc
printAnnotations ans = hsep (map printAnnotation ans)

printAnnotation :: Annotation -> Doc
printAnnotation (Annotation name args) = colon <> colon <+> text name
                                         <> case args of
                                              []        -> empty
                                              otherwise -> parens (commaSepExpr args)

printOp :: Op -> Doc
printOp (Op op) = text op

printSolve :: Solve -> Doc
printSolve Satisfy      = text "satisfy"
printSolve (Minimize e) = text "minimize" <+> printExpr e
printSolve (Maximize e) = text "maximize" <+> printExpr e

printParams :: [Param] -> Doc
printParams ps = commaSep printParam ps

-- Prints the parameters of call expressions (predicates, tests and functions) or annotations
printParam :: Param -> Doc
printParam (i, t, n) = printTypeInst (i, t) <> colon <+> text n

-- Prints the instantiation (var or par) and the type in a variable declaration. If the
-- type is Array or String, it does not print the inst, since these types are of fixed
-- inst. Same with @Ann@ type, but for other reasons.
printTypeInst :: (Inst, Type) -> Doc
printTypeInst (_, t@(Array _ _)) = printType t
printTypeInst (_, String)        = printType String
printTypeInst (_, Ann)           = printType Ann
printTypeInst (i, t)             = printInst i <+> printType t

-- Horizontally concatinates Docs while also putting a comma-space (", ") in between
commaSepDoc :: [Doc] -> Doc
commaSepDoc = hsep . punctuate comma

-- Vertically prints expressions, one per line
-- lineSepExpr :: [Expr] -> Doc
-- lineSepExpr = vcat . map printNakedExpr

-- First, map a function to a list and produce a list of Docs and then apply commaSepDoc
commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f ls = commaSepDoc $ map f ls

-- Special case of commaSep, where f = printNakedExpr
commaSepExpr :: [NakedExpr] -> Doc
commaSepExpr = commaSep printNakedExpr

isAtomic :: NakedExpr -> Bool
isAtomic AnonVar    = True
isAtomic (Var _)    = True
isAtomic (BConst _) = True
isAtomic (IConst _) = True
isAtomic (FConst _) = True
isAtomic (SConst _) = True
isAtomic (SetLit _) = True
isAtomic _          = False

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

putParens :: [Doc] -> [Doc]
putParens []  = []
putParens [x] = [parens x]
putParens xs  = let f = head xs
                    l = last xs
                in text "(" <> f : (init (tail xs)) ++ [l <> text ")"]

punctuateBefore :: Doc -> [Doc] -> [Doc]
punctuateBefore _ []     = []
punctuateBefore p (d:ds) = d : map (p <+>) ds