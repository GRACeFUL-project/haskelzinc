module TimeSpaceConstr.TimeSpaceConstr where

import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences

-- | Constructs an action sequence constraint
--
-- * k = The number of actions
-- * v = The name of the result variable
-- * e = The action sequence expression
actionSequence :: Int -> String -> ASExpr -> ModelData
actionSequence k v e = actionSeqConstraint k v e
