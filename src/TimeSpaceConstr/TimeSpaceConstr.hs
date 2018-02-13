module TimeSpaceConstr.TimeSpaceConstr where

import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences

-- | Constructs an action sequence constraint
--
-- * k = The number of actions
-- * x = The name of the action sequence variable
-- * e = The action sequence expression
actionSequence :: Int -> String -> ASExpr -> ModelData
actionSequence k x e = actionSeqConstraint k x e

-- | Constructs an action sequence cost constraint
--
-- * x = The name of the action sequence variable
-- * c = The name of the resulting cost variable
-- * e = The action sequence cost expression
actionSequenceCost :: String -> String -> ASCostExpr -> ModelData
actionSequenceCost x c e = actionSeqCost x c e

-- | Constructs a list of action sequence cost predicates
--
-- * l = The list of action sequence cost predicate expressions
useCostPreds :: [ASCostPredExpr] -> [ModelData]
useCostPreds l = actionSeqCostPreds l
