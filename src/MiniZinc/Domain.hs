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
-- Supported domains for constraint programming.
--
-----------------------------------------------------------------------------

module MiniZinc.Domain
    ( -- * Domains and types
      Domain, int, float, bool, string, (...)
    ) where

import Data.Data
import MiniZinc.Pretty
import Text.PrettyPrint hiding (int, float)
import qualified Text.PrettyPrint as P

-- | Supported domains
data Domain 
    = Base Type
    | IntRange Int Int
    | FloatRange Float Float
    deriving (Show, Eq, Data, Ord)

-- | Base types
data Type 
    = Boolean
    | Integer
    | Float
    | String
    deriving (Show, Eq, Data, Ord)

-- | Smart constructors
int, float, bool, string :: Domain
int    = Base Integer
float  = Base Float
bool   = Base Boolean
string = Base String

-- | Range operator
class Range a where
    (...) :: a -> a -> Domain

instance Range Int where
    (...) = IntRange

instance Range Float where
    (...) = FloatRange

-- | Pretty printing
instance Pretty Domain where
    pp d = case d of
        Base t         -> pp t
        IntRange n m   -> range P.int n m
        FloatRange n m -> range P.float n m
      where
        range p x y = p x <> text ".." <> p y

instance Pretty Type where
    pp t = text $ case t of
        Boolean -> "boolean"
        Integer -> "int"
        Float   -> "float"
        String  -> "string"
