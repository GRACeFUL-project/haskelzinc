module TimeSpaceConstr.TimeSpaceConstr where

import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences

-- | Constructs an action sequence constraint
--
-- * k = The number of actions
-- * e = The action sequence expression
-- * v = The result variable
actionSequence :: Int -> ASExpr -> Expr -> Expr
actionSequence k e v = actionSeqConstraint k e v
