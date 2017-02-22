{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module MiniZinc.Model 
    (
      -- * Building models
      Model
    , satisfy, maximize
      -- * Constraints
    , (.==.), (./=.), (.>.), (.>=.), (.<.), (.<=.)
    , (.&.)
    , forall
      -- * Domains and types
    , Domain(..), Type(..), int, float, bool, string, (...)
      -- * The @Expr@ type, variable decls
    , Expr, var, array, getId
      -- * Types
    , Ident
      -- * String representation in MiniZinc format
    , emit
    ) where

import Control.Monad
import Data.Array hiding (array)
import Data.Data
import Data.Unique
import Data.List
import Data.Generics.Uniplate.Data
import qualified Data.Set as S
import System.IO.Unsafe (unsafePerformIO)
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
    | IntRange Int Int
    | FloatRange Float Float
    | Set Type Expr
    deriving (Show, Eq, Data, Ord)

data Type 
    = Boolean
    | Integer
    | Float
    | String
    deriving (Show, Eq, Data, Ord)

int, float, bool, string :: Domain
int    = Base Integer
float  = Base Float
bool   = Base Boolean
string = Base String

class Range a where
    (...) :: a -> a -> Domain

instance Range Int where
    (...) = IntRange
instance Range Float where
    (...) = FloatRange

data Constraint 
    = NEQ Expr Expr
    | EQU Expr Expr
    | GTH Expr Expr
    | GTE Expr Expr
    | LTH Expr Expr 
    | LTE Expr Expr
    -- Combinators
    | Constraint :&:   Constraint
    | Constraint :|:   Constraint
    | Constraint :->:  Constraint
    | Constraint :<-:  Constraint
    | Constraint :<->: Constraint
    | Not Constraint
    -- Values
    | T
    | F
    deriving (Show, Eq, Data, Ord)

-- maybe have separate range type
forall :: [Constraint] -> Constraint
forall = foldr (.&.) T

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

-- Smart constructors
T .&. c = c
c .&. T = c
F .&. c = F
c .&. F = F
c .&. d = c :&: d

data Expr 
    -- Variable
    = Var Ident Domain
    -- Values
    | EBool Bool
    | EInt Int
    | EFloat Float
    -- Arithmetic
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
  deriving (Show, Eq, Data, Ord)

var :: Domain -> IO Expr
var d = do 
    n <- fmap (show . hashUnique) newUnique
    return $ Var ("v" ++ n) d

getId :: Expr -> Maybe Ident
getId (Var n _) = Just n
getId _         = Nothing

array :: Ix i => (i, i) -> Domain -> IO (i -> Expr)
array range domain = do 
    a <- fmap (listArray range) $ replicateM (rangeSize range) $ var domain 
    return $ \ i -> a ! i

eval :: Expr -> Expr
eval expr = case expr of
    Add (EInt n) (EInt m) -> EInt (n + m)
    Sub (EInt n) (EInt m) -> EInt (n - m)
    Mul (EInt n) (EInt m) -> EInt (n * m)
    Neg (EInt n)          -> EInt (-n)
    
    Add (EFloat n) (EFloat m) -> EFloat (n + m)
    Sub (EFloat n) (EFloat m) -> EFloat (n - m)
    Mul (EFloat n) (EFloat m) -> EFloat (n * m)
    Div (EFloat n) (EFloat m) -> EFloat (n / m)
    Neg (EFloat n)            -> EFloat (-n)

    _ -> expr

norm :: Expr -> Expr
norm = transform eval

instance Num Expr where
    (+) = Add
    (-) = Sub
    (*) = Mul
    negate = Neg
    fromInteger = EInt . fromInteger

instance Fractional Expr where
    (/) = Div
    fromRational = EFloat . fromRational

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
decvars cs = nub [DecVar n d| Var n d <- universeBi cs]

-- | Pretty printing / translate to minizinc
class Emit a where
    emit :: a -> String

instance Emit Model where
    emit = render . ppModel
instance Emit Expr where
    emit = render . ppExpr . norm

ppModel :: Model -> Doc
ppModel (Model vs cs s) = vcat $
    [decl n d | DecVar n d <- vs] ++
    map ppConstraint cs ++
    [ppProblem s]
  where
    decl n d = text "var" <+> ppDomain d <> colon <+> text n <> semi 

ppDomain :: Domain -> Doc
ppDomain d = case d of
    Base t         -> ppType t
    IntRange n m   -> range P.int n m
    FloatRange n m -> range P.float n m
    Set t e        -> undefined
  where
    range pp x y = pp x <> text ".." <> pp y

ppType :: Type -> Doc
ppType t = text $ case t of
    Boolean -> "boolean"
    Integer -> "int"
    Float   -> "float"
    String  -> "string"

ppConstraint :: Constraint -> Doc
ppConstraint constraint = text "constraint" <+> rec constraint <> semi 
  where
    rec c = case c of
      NEQ x y -> binop x y "!="
      EQU x y -> binop x y "=="
      GTH x y -> binop x y ">"
      GTE x y -> binop x y ">="
      LTH x y -> binop x y "<"
      LTE x y -> binop x y "<="
      x :&: y -> rec x <+> text "/\\" <+> rec y 

    binop x y op = ppExpr x <+> text op <+> ppExpr y 

ppExpr :: Expr -> Doc
ppExpr expr = case expr of
    Var n _  -> text n
    Add x y  -> binop x y "+"
    Sub x y  -> binop x y "-"
    Mul x y  -> binop x y "*"
    Neg x    -> text "-" <> ppExpr x
    EInt x   -> P.int x 
    EBool x  -> text $ if x then "true" else "false"
    EFloat x -> P.float x
  where
    binop x y op = parens $ ppExpr x <+> text op <+> ppExpr y

ppProblem :: Problem -> Doc
ppProblem p = text "solve" <+> problem <> semi
  where
    problem = case p of
        Satisfy       -> text "satisfy"
        Maximize expr -> text "maximize" <+> ppExpr expr
        Minimize expr -> text "minimize" <+> ppExpr expr

ppArray :: (a -> Doc) -> [a] -> Doc
ppArray ppElem = brackets . commaSep . map ppElem

ppSet :: (a -> Doc) -> S.Set a -> Doc
ppSet ppElem = braces . commaSep . map ppElem . S.toList

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate comma
