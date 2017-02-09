{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module MiniZinc.Model 
    (
      -- * Building models
      Model
    , satisfy, maximize
      -- * Constraints
    , (.==.), (./=.), (.>.), (.>=.), (.<.), (.<=.)
      -- * Domains
    , Domain, (...)
      -- * The @Expr@ type
    , Expr((:::)), int, float, bool, string
      -- * Types
    , Ident
      -- * String representation in MiniZinc format
    , toString
    ) where

import Data.Data
import Data.List
import Data.Generics.Uniplate.Data
import Text.PrettyPrint hiding (int, float)
import qualified Text.PrettyPrint as P

-- | Constraint Programming model respresentation
data Model = Model
    { variables   :: [Decl]
    , constraints :: [Constraint]
    , problem     :: Problem
    } deriving (Show, Eq, Data)

type Ident = String

data Decl 
    = DecVar Ident Domain 
    deriving (Show, Eq, Data)

data Domain
    = Base Type  
    | Range Expr Expr
    {-| Set Type [Expr]-}
    {-| Array Type -}
    deriving (Show, Eq, Data)

data Type 
    = Boolean
    | Integer
    | Float
    | String
    deriving (Show, Eq, Data)

(...) :: Expr -> Expr -> Domain
(...) = Range

int, float, bool, string :: Domain
int    = Base Integer
float  = Base Float
bool   = Base Boolean
string = Base String

data Constraint 
    = NEQ Expr Expr
    | EQU Expr Expr
    | GTH Expr Expr
    | GTE Expr Expr
    | LTH Expr Expr
    | LTE Expr Expr
    deriving (Show, Eq, Data)

(.==.) = EQU
infix 4 .==.
(./=.) = NEQ
infix 4 ./=.
(.>.)  = GTH
infix 4 .>.
(.>=.) = GTE
infix 4 .>=.
(.<.)  = LTH
infix 4 .<.
(.<=.) = LTE
infix 4 .<=.

data Expr 
    = Ident ::: Domain
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
    | I Int
    | B Bool
    | F Float
  deriving (Show, Eq, Data)

infix 3 :::

instance Num Expr where
    (+) = Add
    (-) = Sub
    (*) = Mul
    negate = Neg
    fromInteger = I . fromInteger

instance Fractional Expr where
    (/) = Div
    fromRational = F . fromRational

data Problem
    = Satisfy 
    | Maximize Expr
    | Minimize Expr
    deriving (Show, Eq, Data)

-- Building models
satisfy :: [Constraint] -> Model
satisfy cs = Model (decvars cs) cs Satisfy

maximize :: Expr -> [Constraint] -> Model
maximize expr cs = Model (decvars cs) cs (Maximize expr)

minimize :: Expr -> [Constraint] -> Model
minimize expr cs = Model (decvars cs) cs (Minimize expr)

decvars :: [Constraint] -> [Decl]
decvars cs = nub [DecVar n d| n ::: d <- universeBi cs]

-- | Pretty printing / translate to minizinc
toString :: Model -> String
toString = render . ppModel

ppModel :: Model -> Doc
ppModel (Model vs cs s) = vcat $
    [decl n d <> semi | DecVar n d <- vs] ++
    map ppConstraint cs ++
    [ppProblem s]
  where
    decl n d = text "var" <+> ppDomain d <> colon <+> text n

ppDomain :: Domain -> Doc
ppDomain (Base t) = case t of
    Boolean    -> text "boolean"
    Integer    -> text "int"
    Float      -> text "float"
ppDomain d = case d of
    Range x y -> ppExpr x <+> text ".." <+> ppExpr y

ppConstraint :: Constraint -> Doc
ppConstraint constraint = case constraint of
    NEQ x y -> binop x y "!="
    EQU x y -> binop x y "=="
    GTH x y -> binop x y ">"
    GTE x y -> binop x y ">="
    LTH x y -> binop x y "<"
    LTE x y -> binop x y "<="
  where
    binop x y op = text "constraint" <+> ppExpr x <+> text op <+> ppExpr y <> semi

ppExpr :: Expr -> Doc
ppExpr expr = case expr of
    n ::: _ -> text n
    Add x y -> binop x y "+"
    Sub x y -> binop x y "-"
    Mul x y -> binop x y "*"
    Neg x   -> text "-" <> ppExpr x
    I x     -> P.int x 
    B x     -> text $ if x then "true" else "false"
    F x     -> P.float x
  where
    binop x y op = parens $ ppExpr x <+> text op <+> ppExpr y

ppProblem :: Problem -> Doc
ppProblem p = text "solve" <+> problem <> semi
  where
    problem = case p of
        Satisfy       -> text "satisfy"
        Maximize expr -> text "maximize" <+> ppExpr expr
        Minimize expr -> text "minimize" <+> ppExpr expr
