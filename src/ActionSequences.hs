module ActionSequences where
  
import DFA
import qualified Data.Set as S
import Data.Maybe (fromJust)

import Interfaces.MZinHaskell
import Interfaces.MZAST
-- | An action sequence model
data ASModel = ASModel Int      -- ^ The number of actions
                       [ASExpr] -- ^ A list of expressions

-- | An action sequence expression
data ASExpr = Atleast Int         -- ^ The action in question
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

-- | Action i must be performed at least p times in each cell.
--
-- * k = the number of actions
-- * i = the action that has to be repeated
-- * p = the number of times action i has to at least be repeated
atLeast :: Int -> Int -> Int -> DFA
atLeast k i p =
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
atMost :: Int -> Int -> Int -> DFA
atMost k i p =
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
incompatible :: Int -> Int -> Int -> DFA
incompatible k i j =
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
implication :: Int -> Int -> Int -> DFA
implication k i j =
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
value_precedence :: Int -> Int -> Int -> DFA
value_precedence k i j =
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
stretch_min :: Int -> Int -> Int -> DFA
stretch_min k i s =
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
stretch_max :: Int -> Int -> Int -> DFA
stretch_max k i s =
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
or_ctr :: Int -> Int -> Int -> DFA
or_ctr k i j =
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

dfaToRegular :: DFA -> Expr -> Expr
dfaToRegular atm' xs =
  prefCall "regular" [xs, int q,int s,intArray2 d, int q0, intSet f]
  where
    atm = normalize atm'
    q  = S.size (states atm)
    s  = S.size (alphabet atm)
    d  = [[ transition atm state label | label <- [1..s] ] | state <- [1..q]] 
    q0 = start atm
    f  = S.toList (accepting_states atm)

normalize :: DFA -> DFA
normalize d =
  DFA { alphabet         = S.map thetaA (alphabet d)
      , states           = S.map thetaS (states d)
      , accepting_states = S.map thetaS (accepting_states d)
      , transitions      = S.map (\(f,l,t) -> (thetaS f,thetaA l,thetaS t)) (transitions d)
      , start            = thetaS (start d)
      }
  where
    thetaS s = fromJust (lookup s (zip (S.toList (states d)) [1..]))
    thetaA l = fromJust (lookup l (zip (S.toList (alphabet d)) [1..]))

test1 :: String
test1 = layout [constraint (dfaToRegular (atLeast 2 1 1) (Var (Simpl "xs")))]

render :: DFA -> IO ()
render = putStrLn . toString

main :: IO ()
main = render $ or_ctr 4 2 3
