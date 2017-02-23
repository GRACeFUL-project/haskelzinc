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
-- Pretty printing.
--
-----------------------------------------------------------------------------

module MiniZinc.Pretty 
    ( -- * Pretty printing in MiniZinc format
      Pretty, pp
    ) where

import Text.PrettyPrint

class Pretty a where
    pp :: a -> Doc
