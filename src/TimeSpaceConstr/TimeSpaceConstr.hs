{-|
Module      : TimeSpaceConstraints
Description : Interface for constructing time space constraints
License     : BSD3
Maintainer  : Gert-Jan Bottu <gertjan.bottu@kuleuven.be>
Stability   : experimental


This module defines an interface for constructing time space constraints.
Using this module, together with "TimeSpaceConstr.ActionSequences",
one can represent time space models in Haskell code.
-}

module TimeSpaceConstr.TimeSpaceConstr where

import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences

-- | Constructs an action sequence constraint
actionSequence :: Int    -- ^ The number of actions
               -> String -- ^ The name of the action sequence variable
               -> ASExpr -- ^ The action sequence expression
               -> ModelData
actionSequence k x e = actionSeqConstraint k x e

-- | Constructs an action sequence cost constraint
actionSequenceCost :: String     -- ^ The name of the action sequence variable
                   -> String     -- ^ The name of the resulting cost variable
                   -> ASCostExpr -- ^ The action sequence cost expression
                   -> ModelData
actionSequenceCost x c e = actionSeqCost x c e

-- | Constructs a list of action sequence cost predicates
useCostPreds :: [ASCostPredExpr] -- ^ The list of action sequence cost predicate expressions
             -> [ModelData]
useCostPreds l = actionSeqCostPreds l
