{-# LANGUAGE FlexibleInstances #-}

module Interfaces.MonPrint (
  pprint,
  layout
) where

import Interfaces.MonVar (Model(..), Assignment(..), Constraint(..), Output(..), PossibleError(..))
import Interfaces.MZASTBase hiding (Constraint, Output)
import Interfaces.MZBuiltIns (opPrec)
import Text.PrettyPrint
import Data.List

class PrettyPrint a where
  pprint :: a -> Doc

instance PrettyPrint Model where
  pprint m = vcat $ pprint (m_imports m) 
                  : pprint (m_declarations m) 
                  : pprint (m_assignments m)
                  : pprint (m_constraints m) 
                  : pprint (m_solve m)
                  : concat [map pprint $ m_output m]

instance (PrettyPrint a) => PrettyPrint (Maybe a) where
  pprint Nothing  = empty
  pprint (Just a) = pprint a <> semi

instance PrettyPrint [[Char]] where
  pprint [] = empty
  pprint cs = vcat $ text "%% Imports\n" : map pprint cs ++ [emptyline]

instance PrettyPrint [Declaration] where
  pprint [] = empty
  pprint ds = vcat $ text "%% Declarations\n" : map pprint ds ++ [emptyline]

instance PrettyPrint [Assignment] where
  pprint [] = empty
  pprint xs = vcat $ text "%% Assignments\n" : map pprint xs ++ [emptyline]

instance PrettyPrint [Constraint] where
  pprint cs = vcat $ map pprint cs ++ [emptyline]

instance PrettyPrint [Annotation] where
  pprint ans = sep $ map (\a -> colon <> colon <+> pprint a) ans

instance PrettyPrint [Char] where
  pprint str = text str <> semi

instance PrettyPrint Declaration where
  pprint (Declaration nd ans me) = 
    hang (pprint nd) 2 (sep [pprint ans, printBody me]) <> semi

instance PrettyPrint Assignment where
  pprint (Assignment name body) = pprint name <+> printBody (Just body) <> semi

instance PrettyPrint Constraint where
  pprint (Constraint c) = hang (text "constraint") 2 (pprint c <> semi)

instance PrettyPrint Solve where
  pprint s = 
    text "solve"
    <+> case s of
        (Satisfy  ans  ) -> pprint ans <+> text "satisfy"
        (Minimize ans e) -> pprint ans 
                            <+> text "minimize"
                            <+> pprint e
        (Maximize ans e) -> pprint ans 
                            <+> text "maximize"
                            <+> pprint e

instance PrettyPrint Output where
  pprint (Output e) = hang (text "output") 2 (pprint e <> semi)

instance PrettyPrint Annotation where
  pprint (Annotation name args) =
    text name
    <> case args of
       [] -> empty
       xs -> printArgs pprint args

instance PrettyPrint DeclarationSignature where
  pprint (Variable p)          = pprint p
  pprint (Predicate name ps)   = text "predicate"
                                 <+> pprint name
                                 <> parens (pprint ps)
  pprint (Test name ps)        = text "test"
                                 <+> pprint name
                                 <> parens (pprint ps)
  pprint (Function p ps)       = text "function"
                                 <+> pprint p
                                 <> parens (pprint ps)
  pprint (Annotation' name ps) = text "annotation"
                                 <+> text name
                                 <> parens (pprint ps)

instance PrettyPrint AnnExpr where
  pprint (AnnExpr e [])             = pprint e
  pprint (AnnExpr e@(Bi _ _ _) ans) = parens (pprint e) 
                                      <+> pprint ans
  pprint (AnnExpr e@(U _ _) ans)    = parens (pprint e) 
                                      <+> pprint ans
  pprint (AnnExpr e ans)            = pprint e 
                                      <+> pprint ans

instance PrettyPrint Expr where
  pprint AnonVar             = text "_"
  pprint (Var v)             = pprint v
  pprint (BConst b)
    | b                      = text "true"
    | otherwise              = text "false"
  pprint (IConst n)          = int n
  pprint (FConst x)          = float x
  pprint (SConst str)        = doubleQuotes $ text (escape str)
  pprint (SetLit es)         = braces $ commaSepExprs es
  pprint (SetComp e ct)      = braces ( pprint e 
                               <+> text "|" 
                               <+> pprint ct )
  pprint (ArrayLit es)       = brackets $ commaSepExprs es
  pprint (ArrayLit2D ess)    = 
    brackets (foldl1 ($+$) (map (\x -> text "|" <+> commaSepExprs x) ess) <> text "|")
  pprint (ArrayComp e ct)    = brackets (hang (pprint e <+> text "|") 0 (pprint ct))
  pprint (ArrayElem v es)    = pprint v <> brackets (commaSepExprs es)
  pprint (U op e)            = pprint op 
                               <+> (
                                 if isAtomic e 
                                 then pprint e 
                                 else parens (pprint e)
                               )
  pprint (Bi op e1 e2)       = sep [ printParensExpr (opPrec op) e1 
                                   , pprint op 
                                   , printParensExpr (opPrec op) e2
                                   ]
  pprint (Call name args)    = pprint name
                               <> printArgs pprint args
  pprint (ITE (pe:pes) e)    = sep (listIT pe ++ listEIT pes ++ listEI e)
  {-pprint (Let is e)          = text "let" 
                               <+> braces (nest 4 (vcat (map printItem is))) 
                               $+$ text "in" <+> pprint e-}
  pprint (GenCall name ct e) = 
    hang (pprint name <> parens (pprint ct)) 2 (parens (pprint e))

instance PrettyPrint GArguments where
  pprint (A a) = pprint a
  pprint (E e) = pprint e

instance PrettyPrint Ident where
  pprint (Simpl name)  = text name
  pprint (Quoted name) = quotes $ text name

instance PrettyPrint [Param] where
  pprint ps = commaSep pprint ps

instance PrettyPrint Param where
  pprint (i, t, n) = pprint (i, t) <> colon <+> pprint n

instance PrettyPrint CompTail where
  pprint (gs, Nothing) = commaSep pprint gs
  pprint (gs, Just wh) = commaSep pprint gs 
                         <+> text "where" 
                         <+> pprint wh

instance PrettyPrint Generator where
  pprint (es, r) = hsep (punctuate (text ", ") (map pprint es))
                   <+> text "in" 
                   <+> pprint r

instance PrettyPrint Op where
  pprint (Op op) = pprint op

instance PrettyPrint (Inst, Type) where
  pprint (_, t@(Array _ _ _)) = pprint t
  pprint (_, String)          = pprint String
  pprint (_, Ann)             = pprint Ann
  pprint (i, t)               = pprint i <+> pprint t

instance PrettyPrint Inst where
  pprint Dec = text "var"
  pprint Par = text "par"

instance PrettyPrint Type where
  pprint Bool            = text "bool"
  pprint Float           = text "float"
  pprint Int             = text "int"
  pprint String          = text "string"
  pprint (Set t)         = text "set of" <+> pprint t
  pprint (Array ts i ty) = text "array" <> brackets (commaSep pprint ts) 
                              <+> text "of" <+> pprint (i, ty)
  pprint (List i ty)     = text "list of" <+> pprint (i, ty)
  pprint (Opt t)         = text "opt" <+> pprint t
  pprint (Ann)           = text "ann"
  pprint (CT expr)       = pprint expr

-- Errors
instance PrettyPrint IdentError where
  pprint EmptyIdent   = text "Empty identifiers are not allowed."
  pprint SingleQuote  = text "Just a single quote is not an allowed identifier."
  pprint QuoteMissing = text "A quote is missing for a quoted identifier."

instance PrettyPrint PossibleError where
  pprint (IncorrectIdentifier err) = pprint err
  pprint (AssignmentToExpr expr)   = text "Illegal attempt to assign to expression"
                                     $+$ nest 2 (pprint expr)
  pprint (NoVarDS ds)              = text "Following declaration is not a variable"
                                     $+$ nest 2 (pprint ds)
  pprint MultipleSolveItems        = text "Multiple solve items defined."
  pprint NotAllowedInLetDef        = text "Only declaration and constraint items are allowed in a let expression."
  
emptyline :: Doc
emptyline = text ""

layout :: Either PossibleError Model -> String
layout (Right m) = renderStyle defaultStyle (pprint m)
layout (Left m) = undefined

defaultStyle = Style {
  mode = PageMode,
  lineLength = 100,
  ribbonsPerLine = 1.5
}

-- Auxiliary

printBody :: Maybe AnnExpr -> Doc
printBody = maybe empty (\e -> equals <+> (pprint e))

listIT :: (Expr, Expr) -> [Doc]
listIT (e1, e2) = [ text "if" <+> pprint e1
                  , text "then" <+> pprint e2]

listEIT :: [(Expr, Expr)] -> [Doc]
listEIT []            = []
listEIT ((e1, e2):es) = [ text "elseif" <+> pprint e1
                        , text "then" <+> pprint e2]
                        ++ listEIT es 

listEI :: Expr -> [Doc]
listEI e = [ text "else" <+> pprint e
           , text "endif"]

commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f ls = fsep . punctuate comma $ map f ls

commaSepExprs :: [Expr] -> Doc
commaSepExprs = commaSep pprint

printArgs :: (a -> Doc) -> [a] -> Doc
printArgs f args = fcat $ putParens (punctuateBefore comma (map f args))

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

isAtomic :: Expr -> Bool
isAtomic AnonVar         = True
isAtomic (Var _)         = True
isAtomic (BConst _)      = True
isAtomic (IConst _)      = True
isAtomic (FConst _)      = True
isAtomic (SConst _)      = True
isAtomic (SetLit _)      = True
isAtomic (ArrayLit _)    = True
isAtomic (ArrayLit2D _)  = True
isAtomic (ArrayElem _ _) = True
isAtomic (SetComp _ _)   = True
isAtomic (ArrayComp _ _) = True
isAtomic (Call _ _)      = True
isAtomic _               = False

putParens :: [Doc] -> [Doc]
putParens = putBeforeAfter "(" ")"

-- This function is used for placing parentheses in expressions
printParensExpr :: Int -> Expr -> Doc
-- A smaller integer represents higher precedence (tighter binding)
printParensExpr n e@(Bi op _ _)
  | opPrec op <= n   = pprint e
  | otherwise        = parens $ pprint e
printParensExpr _ e  = pprint e

punctuateBefore :: Doc -> [Doc] -> [Doc]
punctuateBefore _ []     = []
punctuateBefore p (d:ds) = d : map (p <+>) ds

putBeforeAfter :: String -> String -> [Doc] -> [Doc]
putBeforeAfter s t []  = []
putBeforeAfter s t [x] = [text s <> parens x <> text t]
putBeforeAfter s t xs  = let f = head xs
                             l = last xs
                         in text s <> f : (init (tail xs)) ++ [l <> text t]