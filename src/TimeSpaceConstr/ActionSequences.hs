{-# LANGUAGE OverloadedStrings #-}

module TimeSpaceConstr.ActionSequences
  ( ASExpr (..)
  , actionSeqConstraint
  , atleast_cells, atmost_cells
  , atleast, atmost
  , incompatible
  , implication
  , value_precedence
  , stretch_min, stretch_max
  , or_as
  , ASCostExpr (..)
  , actionSeqCost
  , uniformCost
  , discountCost
  , dependentCost
  , ASCostPredExpr (..)
  , actionSeqCostPreds
  , uniformCostPred
  , discountCostPred
  , dependentCostPred
  ) where

import TimeSpaceConstr.DFA
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.String

-- import Interfaces.MZinHaskell
import Interfaces.MZAST
import Interfaces.MZBuiltIns
-- import Interfaces.FZSolutionParser (Solution)
-- import Text.Parsec.Error
import Interfaces.MZASTBase

type Label = Int
type State = Int


-- -----------------------------------------------------------------
-- ACTION SEQUENCE EXPRESSIONS
-- -----------------------------------------------------------------

-- | An action sequence expression
data ASExpr = AtleastCells Int    -- ^ The minimum amount of cells in the sequence
            | AtmostCells Int     -- ^ The maximum amount of cells in the sequence
            | Atleast Int         -- ^ The action in question
                      Int         -- ^ The min number of times this action has to be performed
            | Atmost Int          -- ^ The action in question
                     Int          -- ^ The max number of times this action can be performed
            | Incompatible Int    -- ^ The first of the incompatible actions
                           Int    -- ^ The second of the incompatible actions
            | Implication Int     -- ^ The action that implies the second action
                          Int     -- ^ The action that is implied
            | ValuePrecedence Int -- ^ The action that has to precede the second action
                              Int -- ^ The action that has to be preceded by the first action
            | StretchMin Int      -- ^ The action in question
                         Int      -- ^ The min number of times the action has to be performed in a row
                                  -- once it has been performed at least once
            | StretchMax Int      -- ^ The action in question
                         Int      -- ^ The max number of times the action may be performed in a row
            | Or Int              -- ^ The first of the two actions, at least one of which has to be performed
                 Int              -- ^ The second of the two actions, at least one of which has to be performed
  deriving (Show)

-- | Constructors
atleast_cells :: Int -> ASExpr
atleast_cells = AtleastCells

atmost_cells :: Int -> ASExpr
atmost_cells = AtmostCells

atleast :: Int -> Int -> ASExpr
atleast = Atleast

atmost :: Int -> Int -> ASExpr
atmost = Atmost

incompatible :: Int -> Int -> ASExpr
incompatible = Incompatible

implication :: Int -> Int -> ASExpr
implication = Implication

value_precedence :: Int -> Int -> ASExpr
value_precedence = ValuePrecedence

stretch_min :: Int -> Int -> ASExpr
stretch_min = StretchMin

stretch_max :: Int -> Int -> ASExpr
stretch_max = StretchMax

or_as :: Int -> Int -> ASExpr
or_as = Or

-- | Transform an action sequence expression into a DFA
--
-- * k = the number of actions
-- * e = the action sequence expression
asExprToDFA :: Int -> ASExpr -> DFA
asExprToDFA k e = case e of
  AtleastCells i      -> constr_atLeastCells k i
  AtmostCells i       -> constr_atMostCells k i
  Atleast i p         -> constr_atLeast k i p
  Atmost i p          -> constr_atMost k i p
  Incompatible i j    -> constr_incompatible k i j
  Implication i j     -> constr_implication k i j
  ValuePrecedence i j -> constr_value_precedence k i j
  StretchMin i s      -> constr_stretch_min k i s
  StretchMax i s      -> constr_stretch_max k i s
  Or i j              -> constr_or k i j


-- -----------------------------------------------------------------
-- AUTOMATAS
-- -----------------------------------------------------------------

-- | The sequence must contain at least i cells.
--
-- * k = the number of actions
-- * c = the minimum amount of cells in the sequence
constr_atLeastCells :: Int -> Int -> DFA
constr_atLeastCells k c =
  DFA
   { alphabet         = S.fromList abc
   , states           = S.fromList [0..i+2]
   , accepting_states = S.fromList [i,padding]
   , transitions      =           S.fromList [(q,next,q+1) | q <- [0..i-1]]
                        `S.union` S.fromList [(q,a,q)   | q <- [0..i], a <- [1..k]]
                        `S.union` S.singleton (i,next,i)
                        `S.union` S.fromList ((i,nop,padding) : [(q,nop,failure) | q <- [0..i-1]])
                        `S.union` S.fromList [(failure,l,failure) | l <- abc]
                        `S.union` S.fromList ((padding,nop,padding) : [(padding,l,failure) | l <- abc, l /= nop])
   , start            = 0
   , failure          = failure
   }
   where
     i = c - 1 -- The min number of next actions

     abc = [1..k+2]

     next = k + 1
     nop  = k + 2

     failure  = i + 2
     padding  = i + 1

-- | The sequence can contain at most i cells.
--
-- * k = the number of actions
-- * c = the maximum amount of cells in the sequence
constr_atMostCells :: Int -> Int -> DFA
constr_atMostCells k c =
  DFA
   { alphabet         = S.fromList abc
   , states           = S.fromList [0..i+2]
   , accepting_states = S.fromList (padding:[0..i])
   , transitions      =           S.fromList [(q,next,q+1) | q <- [0..i-1]]
                        `S.union` S.fromList [(q,j,q)   | q <- [0..i], j <- [1..k]]
                        `S.union` S.fromList [(q,nop,padding) | q <- [0..i]]
                        `S.union` S.singleton (i,next,failure)
                        `S.union` S.fromList [(failure,l,failure) | l <- abc]
                        `S.union` S.fromList ((padding,nop,padding) : [(padding,l,failure) | l <- abc, l /= nop])
   , start            = 0
   , failure          = failure
   }
   where
     i = c - 1 -- The max number of next actions

     abc = [1..k+2]

     next = k + 1
     nop  = k + 2

     failure  = i + 2
     padding  = i + 1

-- | Action i must be performed at least p times in each cell.
--
-- * k = the number of actions
-- * i = the action that has to be repeated
-- * p = the number of times action i has to at least be repeated
constr_atLeast :: Int -> Int -> Int -> DFA
constr_atLeast k i p =
  DFA
   { alphabet         = S.fromList abc
   , states           = S.fromList [0..p+2]
   , accepting_states = S.fromList [p,padding]
   , transitions      =           S.fromList [(q,i,q+1) | q <- [0..p-1]]
                        `S.union` S.fromList [(q,j,q)   | q <- [0..p-1], j <- [1..k], j /= i]
                        `S.union` S.fromList (concat [[(q,next,failure),(q,nop,failure)] | q <- [0..p-1]])
                        `S.union` S.fromList ((p,next,0) : (p,nop,padding) : [(p,l,p) | l <- [1..k]]) 
                        `S.union` S.fromList [(failure,l,failure) | l <- abc]
                        `S.union` S.fromList ((padding,nop,padding) : [(padding,l,failure) | l <- abc, l /= nop])
   , start            = 0
   , failure          = failure
   }
   where
     abc = [1..k+2]

     next = k + 1
     nop  = k + 2

     failure  = p + 2
     padding  = p + 1

-- | Action i has to performed at most p times in each cell.
--
-- * k = the number of actions
-- * i = the action that has to be repeated
-- * p = the number of times action i can at most be repeated
constr_atMost :: Int -> Int -> Int -> DFA
constr_atMost k i p =
  DFA
   { alphabet         = S.fromList abc
   , states           = S.fromList [0..p+2]
   , accepting_states = S.fromList (padding:[0..p])
   , transitions      =           S.fromList [(q,i,q+1) | q <- [0..p-1]]
                        `S.union` S.fromList [(q,j,q)   | q <- [0..p], j <- [1..k], j /= i]
                        `S.union` S.fromList (concat [[(q,next,0),(q,nop,padding)] | q <- [0..p]])
                        `S.union` S.singleton (p,i,failure)
                        `S.union` S.fromList [(failure,l,failure) | l <- abc]
                        `S.union` S.fromList ((padding,nop,padding) : [(padding,l,failure) | l <- abc, l /= nop])
   , start            = 0
   , failure          = failure
   }
   where
     abc = [1..k+2]
     
     next = k + 1
     nop  = k + 2
     
     failure  = p + 2
     padding  = p + 1

-- | Actions i and j cannot be performed in the same cell.
--
-- * k = the number of actions
-- * i = the action that cannot be combined with action j
-- * j = the action that cannot be combined with action i
constr_incompatible :: Int -> Int -> Int -> DFA
constr_incompatible k i j =
  DFA
  { alphabet         = S.fromList abc
  , states           = S.fromList [0..4]
  , accepting_states = S.fromList [0..3]
  , transitions      =           S.fromList [(0,a,0) | a <- [1..k], a /= i, a /= j]
                       `S.union` S.singleton (0,i,1)
                       `S.union` S.singleton (0,j,2)
                       `S.union` S.fromList [(1,a,1) | a <- [1..k], a /= j]
                       `S.union` S.fromList [(2,a,2) | a <- [1..k], a /= i]
                       `S.union` S.singleton (1,j,failure)
                       `S.union` S.singleton (2,i,failure)
                       `S.union` S.fromList [(p,next,0) | p <- [0..2]]
                       `S.union` S.fromList [(p,nop,padding) | p <- [0..2]]
                       `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- abc, a /= nop])
                       `S.union` S.fromList [(failure,a,failure) | a <- abc]
  , start            = 0
  , failure          = failure
  }
  where
    abc = [1..k+2]

    next = k + 1
    nop  = k + 2

    failure  = 4
    padding  = 3

-- | Action i implies action j.
-- This means that if action i is performed in a cell,
-- action j has to be performed as well.
--
-- * k = the number of actions
-- * i = the action that implies action j
-- * j = the action that is implied by action i
constr_implication :: Int -> Int -> Int -> DFA
constr_implication k i j =
  DFA
  { alphabet         = S.fromList abc
  , states           = S.fromList [0..3]
  , accepting_states = S.fromList [0,2]
  , transitions      =           S.fromList [(0,a,0) | a <- [1..k], a /= i]
                       `S.union` S.fromList [(1,a,1) | a <- [1..k], a /= i, a /= j]
                       `S.union` S.singleton (0,i,1)
                       `S.union` S.singleton (1,j,0)
                       `S.union` S.singleton (1,i,failure)
                       `S.union` S.singleton (0,next,0)
                       `S.union` S.singleton (0,nop,padding)
                       `S.union` S.singleton (1,nop,failure)
                       `S.union` S.singleton (1,next,failure)
                       `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- abc, a /= nop])
                       `S.union` S.fromList [(failure,a,failure) | a <- abc]
  , start            = 0
  , failure          = failure
  }
  where
    abc = [1..k+2]

    next = k + 1
    nop  = k + 2

    failure  = 3
    padding  = 2

-- | Action i has to precede action j.
-- This means that in order for action j to be performed,
-- action i has to be performed at least once.
--
-- * k = the number of actions
-- * i = the action that to precede action j
-- * j = the action that has to be preceded by action i
constr_value_precedence :: Int -> Int -> Int -> DFA
constr_value_precedence k i j =
  DFA
  { alphabet         = S.fromList abc
  , states           = S.fromList [0..3]
  , accepting_states = S.fromList [0..2]
  , transitions      =           S.fromList [(0,a,0) | a <- [1..k], a /= i, a /= j]
                       `S.union` S.fromList [(1,a,1) | a <- [1..k]]
                       `S.union` S.singleton (0,i,1)
                       `S.union` S.singleton (0,j,failure)
                       `S.union` S.fromList [(p,next,0) | p <- [0..1]]
                       `S.union` S.fromList [(p,nop,padding) | p <- [0..1]]
                       `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- abc, a /= nop])
                       `S.union` S.fromList [(failure,a,failure) | a <- abc]
  , start            = 0
  , failure          = failure
  }
  where
    abc = [1..k+2]

    next = k + 1
    nop  = k + 2

    failure  = 3
    padding  = 2

-- | When an action i is performed at least once,
-- it has to be performed at least s times in a row.
--
-- * k = the number of actions
-- * i = the action in question
-- * s = the number of times action i has to at least be performed in a row
--       once it is performed at least once
constr_stretch_min :: Int -> Int -> Int -> DFA
constr_stretch_min k i s =
  DFA
  { alphabet         = S.fromList abc
  , states           = S.fromList [0..s+2]
  , accepting_states = S.fromList [0,s,padding]
  , transitions      =           S.fromList [(0,a,0) | a <- [1..k], a /= i]
                       `S.union` S.fromList [(p,i,p+1) | p <- [0..s-1]]
                       `S.union` S.fromList ((s,i,s) : [(s,a,0) | a <- [1..k], a /= i])
                       `S.union` S.fromList [(p,a,failure) | p <- [1..s-1], a <- abc, a /= i]
                       `S.union` S.singleton (0,next,0)
                       `S.union` S.singleton (s,next,0)
                       `S.union` S.singleton (0,nop,padding)
                       `S.union` S.singleton (s,nop,padding)
                       `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- abc, a /= nop])
                       `S.union` S.fromList [(failure,a,failure) | a <- abc]
  , start            = 0
  , failure          = failure
  }
  where
    abc = [1..k+2]

    next = k + 1
    nop  = k + 2

    failure  = s + 2
    padding  = s + 1

-- | When an action i is performed at least once,
-- it may be performed at most s times in a row.
--
-- * k = the number of actions
-- * i = the action in question
-- * s = the number of times action i may at most be performed in a row
--       once it is performed at least once
constr_stretch_max :: Int -> Int -> Int -> DFA
constr_stretch_max k i s =
  DFA
  { alphabet         = S.fromList abc
  , states           = S.fromList [0..s+2]
  , accepting_states = S.fromList (padding : [0..s])
  , transitions      =           S.fromList [(p,a,0) | p <- [0..s], a <- [1..k], a /= i]
                       `S.union` S.fromList [(p,i,p+1) | p <- [0..s-1]]
                       `S.union` S.singleton (s,i,failure)
                       `S.union` S.fromList [(p,next,0) | p <- [0..s]]
                       `S.union` S.fromList [(p,nop,padding) | p <- [0..s]]
                       `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- abc, a /= nop])
                       `S.union` S.fromList [(failure,a,failure) | a <- abc]
  , start            = 0
  , failure          = failure
  }
  where
    abc = [1..k+2]

    next = k + 1
    nop  = k + 2

    failure  = s + 2
    padding  = s + 1

-- | At least one of the two action i and j must be performed,
-- in every cell
--
-- * k = the number of actions
-- * i = the first of the pair of actions, one of which at least has to be performed
-- * j = the second of the pair of actions, one of which at least has to be performed
constr_or :: Int -> Int -> Int -> DFA
constr_or k i j =
  DFA
  { alphabet         = S.fromList abc
  , states           = S.fromList [0..3]
  , accepting_states = S.fromList [1,2]
  , transitions      =           S.fromList [(0,a,0) | a <- [1..k], a /= i, a /= j]
                       `S.union` S.singleton (0,i,1)
                       `S.union` S.singleton (0,j,1)
                       `S.union` S.fromList [(1,a,1) | a <- [1..k]]
                       `S.union` S.singleton (0,nop,failure)
                       `S.union` S.singleton (0,next,failure)
                       `S.union` S.singleton (1,nop,padding)
                       `S.union` S.singleton (1,next,0)
                       `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- abc, a /= nop])
                       `S.union` S.fromList [(failure,a,failure) | a <- abc]
  , start            = 0
  , failure          = failure
  }
  where
    abc = [1..k+2]

    next = k + 1
    nop  = k + 2

    failure  = 3
    padding  = 2

dfaToRegular :: ImplDFA -> Expr -> Expr
dfaToRegular atm xs =
  prefCall "regular" [xs, int q,int s,intArray2 d, int q0, intSet f]
  where
    q  = S.size (statesI atm) - 1
    s  = S.size (alphabetI atm)
    d  = [[ transitionI atm state label | label <- [1..s] ] | state <- [1..q]]
    q0 = startI atm
    f  = S.toList (accepting_statesI atm)


-- -----------------------------------------------------------------
-- ACTION SEQUENCE COST EXPRESSIONS
-- -----------------------------------------------------------------

-- | An action sequence cost expression
data ASCostExpr = UniformCost Int    -- ^ The action for which the cost is given
                              Int    -- ^ The uniform cost for this action
                | DiscountCost Int   -- ^ The action for which the cost is given
                               Int   -- ^ The action which moves to the next station
                               Int   -- ^ The initial cost
                               Int   -- ^ The discounted cost
                               Bool  -- ^ The local flag
                | DependentCost Int  -- ^ The action for which the cost is given
                                Int  -- ^ The action which influences the cost
                                Int  -- ^ The action which moves to the next station
                                Int  -- ^ The original cost
                                Int  -- ^ The discounted cost
                                Bool -- ^ The local flag

-- | Constructors
uniformCost :: Int -> Int -> ASCostExpr
uniformCost = UniformCost

discountCost :: Int -> Int -> Int -> Int -> Bool -> ASCostExpr
discountCost = DiscountCost

dependentCost :: Int -> Int -> Int -> Int -> Int -> Bool -> ASCostExpr
dependentCost = DependentCost

-- | Transform an action sequence cost expression into a HaskellZinc expression
--
-- * x = the action sequence variable
-- * r = the variable containing the resulting cost
-- * e = the cost expression
asCostExprToExpr :: Expr -> Expr -> ASCostExpr -> Expr
asCostExprToExpr x r e = case e of
  UniformCost a c           -> prefCall "uniform"   [x, int a, int c, r]
  DiscountCost a n c d l    -> prefCall "discount"  [x, int a, int n, int c, int d, bool l, r]
  DependentCost a b n c d l -> prefCall "dependent" [x, int a, int b, int n, int c, int d, bool l, r]

-- | An action sequence cost predicate expression
data ASCostPredExpr = UniformCostPred
                    | DiscountCostPred
                    | DependentCostPred

-- | Constructors
uniformCostPred :: ASCostPredExpr
uniformCostPred = UniformCostPred

discountCostPred :: ASCostPredExpr
discountCostPred = DiscountCostPred

dependentCostPred :: ASCostPredExpr
dependentCostPred = DependentCostPred

-- | Transform an action sequence cost predicate expression into a predicate
--
-- * e = the action sequence cost predicate expression
asCostPredExprToPred :: ASCostPredExpr -> ModelData
asCostPredExprToPred e = case e of
  UniformCostPred   -> uniform_cost_pred
  DiscountCostPred  -> discount_cost_pred
  DependentCostPred -> dependent_cost_pred


-- -----------------------------------------------------------------
-- COST PREDICATES
-- -----------------------------------------------------------------

-- | Generate the predicate for uniform cost.
-- The given action has a constant cost.
-- The given variable v gets constrained to be the total cost
-- of the actions in sequence x
--
-- * x      = the sequence of actions
-- * action = the action for which the cost is always the constant cost
-- * cost   = the constant cost, corresponding to action action
-- * result = the variable which gets constrained to be the total cost
--            of the actions in sequence x
uniform_cost_pred :: ModelData
uniform_cost_pred =
  predicate "uniform"[ var (Array [Int] Dec Int) "x"
                     , par Int "action"
                     , par Int "cost"
                     , var Int "result"
                     ]
  =. let_ [
         par Int "x_length"           =. mz_length["x"],
         par Int "result_upper_bound" =. "x_length" * "cost",
         var (Array [CT $ 0..."x_length"] Dec (CT $ 0..."result_upper_bound")) "counters"
          ]
    ("counters"!.[0] =.= 0
     /\. forall [["i"] @@ 1..."x_length"] "forall" (
           if_     ("x"!.["i"] =.= "action")
           `then_` ("counters"!.["i"] =.= "counters"!.["i" - 1] + "cost")
           `else_` ("counters"!.["i"] =.= "counters"!.["i" - 1]))
     /\. "result" =.= "counters"!.["x_length"])

-- | Generate the predicate for discount cost.
-- The first time the given action is executed,
-- it has cost cost.
-- All the executions following, will cost discount_cost,
-- for discount_cost < cost.
--
-- * x                  = the sequence of actions
-- * action             = the action for which the cost is given
-- * cell_change_action = the action which moves to the next station
-- * cost               = the initial cost for the given action action
-- * discount_cost      = the discounted cost,
--                        after the action has already been executed once
-- * local_flag         = the flag marking whether each station again
--                        starts with cost cost.
--                        If the flag is false, the cost cost will be
--                        charged max once in the whole sequence x
-- * result             = the variable which gets constrained to be
--                        the total cost of the actions in sequence x
discount_cost_pred :: ModelData
discount_cost_pred =
  predicate "discount"[ var (Array [Int] Dec Int) "x"
                      , par Int  "action"
                      , par Int  "cell_changing_action"
                      , par Int  "cost"
                      , par Int  "discount_cost"
                      , par Bool "local_flag"
                      , var Int  "result"
                      ]
  =. let_ [
         par Int "x_length"           =. mz_length["x"],
         par Int "result_upper_bound" =. "x_length" * "cost",
         var (Array [CT $ 0..."x_length"] Dec (CT $ 0..."result_upper_bound")) "counters",
         var (Array [CT $ 0..."x_length"] Dec Bool) "saw_action"
          ]
  ("counters"!.[0] =.= 0
  /\. "saw_action"!.[0] =.= false
  /\. forall [["i"] @@ 1..."x_length"] "forall" (
        if_     ("x"!.["i"] =.= "action")
        `then_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                 + "cost" * (1 - (mz_bool2int ["saw_action"!.["i" - 1]]))
                 + "discount_cost" * (mz_bool2int ["saw_action"!.["i" - 1]])
                /\. "saw_action"!.["i"] =.= true)
        `else_` (if_     ("x"!.["i"] =.= "cell_changing_action" /\. "local_flag")
                 `then_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                         /\. "saw_action"!.["i"] =.= false)
                 `else_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                         /\. "saw_action"!.["i"] =.= "saw_action"!.["i" - 1])))
  /\. "result" =.= "counters"!.["x_length"])

-- | Generate the predicate for action dependent cost.
-- The action action gets uniform cost cost when cost_changing_action
-- is not performed.
-- If it does get execute, action gets the uniform cost discount_cost,
-- for discount_cost < cost.
--
-- * x                    = the sequence of actions
-- * action               = the action for which the cost is given
-- * cost_changing_action = the action which influences the cost of action action
-- * cell_changing_action = the action which moves to the next station
-- * cost                 = the original cost for action action
-- * discount_cost        = the cost for action action,
--                          if cost_changing_action is executed as well
-- * local_flag           = the flag marking whether cost_changing_action works
--                          locally in a single station or globally over
--                          the whole sequence x
-- * result               = the variable which gets constrained to be the total cost
--                          of the actions in sequence x
dependent_cost_pred :: ModelData
dependent_cost_pred =
  predicate "dependent"[ var (Array [Int] Dec Int) "x"
                       , par Int  "action"
                       , par Int  "cost_changing_action"
                       , par Int  "cell_changing_action"
                       , par Int  "cost"
                       , par Int  "discount_cost"
                       , par Bool "local_flag"
                       , var Int  "result"
                       ]
  =. let_ [
         par Int "x_length" =. mz_length["x"],
         par Int "result_upper_bound" =. "x_length" * "cost",
         var (Array [CT $ 0..."x_length"] Dec (CT $ 0..."result_upper_bound")) "counters",
         var (Array [CT $ 0..."x_length"] Dec Bool) "saw_cost_changing_action",
         var (Array [CT $ 0..."x_length"] Dec Int) "nb_action"
          ]
  ("counters"!.[0] =.= 0
  /\. "saw_cost_changing_action"!.[0] =.= false
  /\. "nb_action"!.[0] =.= 0
  /\. forall [["i"] @@ 1..."x_length"] "forall" (
        if_     ("x"!.["i"] =.= "action")
        `then_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                 + "cost" * (1 - (mz_bool2int ["saw_cost_changing_action"!.["i" - 1]]))
                 + "discount_cost" * (mz_bool2int ["saw_cost_changing_action"!.["i" - 1]])
                /\. "nb_action"!.["i"] =.= "nb_action"!.["i" - 1] + 1
                /\. "saw_cost_changing_action"!.["i"] =.= "saw_cost_changing_action"!.["i" - 1])
        `else_` (if_     ("x"!.["i"] =.= "cost_changing_action"
                          /\. not_ ("saw_cost_changing_action"!.["i" - 1]))
                 `then_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                           + "nb_action"!.["i" - 1] * ("discount_cost" - "cost")
                         /\. "nb_action"!.["i"] =.= "nb_action"!.["i" - 1]
                         /\. "saw_cost_changing_action"!.["i"] =.= true)
                 `else_` (if_     ("x"!.["i"] =.= "cell_changing_action"
                                   /\. "local_flag")
                          `then_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                                  /\. "nb_action"!.[0] =.= 0
                                  /\. "saw_cost_changing_action"!.["i"] =.= false)
                          `else_` ("counters"!.["i"] =.= "counters"!.["i" - 1]
                                  /\. "nb_action"!.["i"] =.= "nb_action"!.["i" - 1]
                                  /\. "saw_cost_changing_action"!.["i"] =.= "saw_cost_changing_action"!.["i" - 1]))))
  /\. "result" =.= "counters"!.["x_length"])


-- -----------------------------------------------------------------
-- MAIN
-- -----------------------------------------------------------------

-- | The main method of this module for constructing action sequence constraints.
-- Takes the action sequence expression and produces a HaskellZinc expression.
--
-- * k = The number of actions
-- * x = The name of the action sequence variable
-- * e = The action sequence expression
actionSeqConstraint :: Int -> String -> ASExpr -> ModelData
actionSeqConstraint k x e = constraint $ dfaToRegular (dfaToImplDFA (asExprToDFA k e)) $ str2var x

-- | The main method of this module for constructing cost constraints.
-- Takes the cost expression and produces a HaskellZinc expression.
--
-- * x = The name of the action sequence variable
-- * c = The name of the variable containing the resulting cost
-- * e = The cost expression
actionSeqCost :: String -> String -> ASCostExpr -> ModelData
actionSeqCost x c e = constraint $ asCostExprToExpr (str2var x) (str2var c) e

-- | The main method for constructing cost predicates.
-- Takes a list of cost predicate expressions and produces a list
-- of HaskellZinc predicates.
--
-- * l = The list of cost predicate expressions
actionSeqCostPreds :: [ASCostPredExpr] -> [ModelData]
actionSeqCostPreds l = map asCostPredExprToPred l

-- | Converts a string to a HaskellZinc variable
str2var :: String -> Expr
str2var = Var . Simpl
