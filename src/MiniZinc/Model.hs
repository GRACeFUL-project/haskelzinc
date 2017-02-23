{-# LANGUAGE DeriveDataTypeable #-}
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
-- A small (proof-of-concept) DSL for constraint programming model.
--
-----------------------------------------------------------------------------

module MiniZinc.Model 
    ( -- * Building models
      Model
    , satisfy, maximize, minimize
      -- * Emit CP model as string
    , emit
    ) where

import MiniZinc.Constraint
import MiniZinc.Domain
import MiniZinc.Pretty

import Data.Data
import Data.Generics.Uniplate.Data
import Text.PrettyPrint hiding (int, float)

-- | Constraint Programming model respresentation
data Model = Model
    { declarations :: [(Ident, Domain)]
    , constraints  :: [Constraint]
    , problem      :: Problem
    } deriving (Show, Eq, Data)

data Problem
    = Satisfy 
    | Maximize Expr
    | Minimize Expr
    deriving (Show, Eq, Data)

-- | Building models
satisfy :: [Constraint] -> Model
satisfy cs = Model (getVars cs) cs Satisfy

maximize :: Expr -> [Constraint] -> Model
maximize expr cs = Model (getVars cs) cs (Maximize expr)

minimize :: Expr -> [Constraint] -> Model
minimize expr cs = Model (getVars cs) cs (Minimize expr)

-- | Pretty printing / translate to minizinc
emit :: Pretty a => a -> String
emit = render . pp

instance Pretty Model where
    pp (Model vs cs s) = vcat $ map decl vs ++ map pp cs ++ [pp s]
      where
        decl (n, d) = text "var" <+> pp d <> colon <+> text n <> semi 

instance Pretty Problem where
    pp p = text "solve" <+> problem <> semi
      where
        problem = case p of
            Satisfy       -> text "satisfy"
            Maximize expr -> text "maximize" <+> pp expr
            Minimize expr -> text "minimize" <+> pp expr
