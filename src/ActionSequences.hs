module ActionSequences where
  
import DFA
import qualified Data.Set as S
import Data.Maybe (fromJust)

import Interfaces.MZinHaskell
import Interfaces.MZAST

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
