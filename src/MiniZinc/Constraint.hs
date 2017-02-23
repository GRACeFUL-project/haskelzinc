{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-----------------------------------------------------------------------------
-- Copyright 2017, GRACeFUL project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alexg@chalmers.se
-- Stability   :  experimental
-- Portability :  portable (depends on ghc)
--
-- Constraints.
--
-----------------------------------------------------------------------------

module MiniZinc.Constraint 
    ( -- * Constraints
      (.==.), (./=.), (.>.), (.>=.), (.<.), (.<=.)
      -- * Constraint combinators
    , (.&.), forall
      -- * The @Expr@ type, variable decls
    , Expr, var, array, getId, getVars
      -- * Types
    , Constraint, Ident
    ) where

import MiniZinc.Domain (Domain)
import MiniZinc.Pretty

import Control.Monad
import Data.Array hiding (array)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.Unique
import Text.PrettyPrint 

type Ident = String

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

-- TODO add smart constructors for the other combinators

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

getVars :: Data a => a -> [(Ident, Domain)]
getVars x = nub [(n, d) | (Var n d) <- universeBi x]

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

instance Pretty Constraint where
    pp constraint = text "constraint" <+> rec constraint <> semi 
      where
        rec c = case c of
          NEQ x y -> binop x y "!="
          EQU x y -> binop x y "=="
          GTH x y -> binop x y ">"
          GTE x y -> binop x y ">="
          LTH x y -> binop x y "<"
          LTE x y -> binop x y "<="
          x :&: y -> rec x <+> text "/\\" <+> rec y 

        binop x y op = pp x <+> text op <+> pp y 

instance Pretty Expr where
    pp expr = rec expr'
      where
        expr' = norm expr

        rec e = case e of
          Var n _  -> text n
          Add x y  -> binop x y "+"
          Sub x y  -> binop x y "-"
          Mul x y  -> binop x y "*"
          Neg x    -> text "-" <> rec x
          EInt x   -> int x 
          EBool x  -> text $ if x then "true" else "false"
          EFloat x -> float x

        binop x y op = parens $ rec x <+> text op <+> rec y
