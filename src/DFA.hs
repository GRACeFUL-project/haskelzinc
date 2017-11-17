module DFA
 ( DFA (..)
 , minimize
 , sequence
 , toString
 , transition
 ) where

import qualified NFA as N

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (findIndex,elemIndex,delete)

import Prelude hiding (sequence)

import Text.Dot

type Label = Int
type State = Int

-- | The representation of a DFA.
--
-- * alphabet    = the possible actions
-- * states      = the states of the DFA
-- * accepting_states
-- * transitions = the possible transitions, of the form
--                 <current state, action, new state>
-- * start       = the start state
-- * failure     = the failure state
data DFA = 
  DFA { alphabet         :: S.Set Label
      , states           :: S.Set State
      , accepting_states :: S.Set State
      , transitions      :: S.Set (State,Label,State)
      , start            :: State
      , failure          :: State
      } deriving Show

-- | The representation of a DFA with implicit failure state 0.
--
-- * alphabet    = the possible actions
-- * states      = the states of the DFA
-- * accepting_states
-- * transitions = the possible transitions, of the form
--                 <current state, action, new state>
-- * start       = the start state
data ImplDFA =
  ImplDFA { alphabetI         :: S.Set Label
          , statesI           :: S.Set State
          , accepting_statesI :: S.Set State
          , transitionsI      :: S.Set (State,Label,State)
          , startI            :: State
          } deriving Show

transition :: DFA -> State -> Label -> State
transition d s l = head [t' | (s',l',t') <- S.toList (transitions d), s' == s, l' == l]

transitionI :: ImplDFA -> State -> Label -> State
transitionI d s l = head [t' | (s',l',t') <- S.toList (transitionsI d), s' == s, l' == l]

example = 
  DFA { alphabet         = S.fromList [0,1]
      , states           = S.fromList [1..6]
      , accepting_states = S.fromList [3,4,5]
      , transitions      = 
          S.fromList [(1,0,2),(1,1,3)
                     ,(2,0,1),(2,1,4)
                     ,(3,0,5),(3,1,6)
                     ,(4,0,5),(4,1,6)
                     ,(5,0,5),(5,1,6)
                     ,(6,0,6),(6,1,6)
                     ]
      , start            = 1
      , failure          = 6
      }

-- | Minimize the given DFA.
--
-- This returns an equivalent DFA with a minimal number of states.
-- The implementation is based on Hopcroft's algoritm.
minimize :: DFA -> DFA
minimize dfa = update reduce
  where 
    reduce :: [S.Set State]
    reduce = go1 [accepting_states dfa] [accepting_states dfa,S.difference (states dfa) (accepting_states dfa)] 
     where 
      go1 :: [S.Set State] -> [S.Set State] -> [S.Set State]
      go1 w p = go1' (filter (not . S.null) w) p

      go1' []    p = p
      go1' (a:w) p = go2 (S.toList (alphabet dfa)) a w p 

      go2 :: [Label] -> S.Set State -> [S.Set State] -> [S.Set State] -> [S.Set State]
      go2 []     _ w p = go1 w p
      go2 (c:cs) a w p = 
        let x = S.map (\(f,_,_) -> f) (S.filter (\(_,l,t) -> l == c && S.member t a) (transitions dfa))
        in go3 p x cs a w []

      go3 :: [S.Set State] -> S.Set State -> [Label] -> S.Set State -> [S.Set State] -> [S.Set State] -> [S.Set State]
      go3 []     x cs a w p =  go2 cs a w p
      go3 (y:p') x cs a w p = 
         let cap = S.intersection x y
             dif = S.difference   y x
             p2 | not (S.null cap), not (S.null dif)  =  (cap:dif:p)
                | otherwise                           =  (y:p) 
             w2 | y `elem` w                          =   cap : dif : delete y w
                | S.size cap <= S.size dif            =   cap : w
                | otherwise                           =   dif : w
         in go3 p' x cs a w2 p2

    update :: [S.Set State] -> DFA
    update p = 
      DFA { alphabet         = alphabet dfa
          , states           = S.map theta (states dfa)
          , accepting_states = S.map theta (accepting_states dfa)
          , transitions      = S.map (\(f,l,t) -> (theta f,l,theta t)) (transitions dfa)
          , start            = theta (start dfa)
          , failure          = theta (failure dfa)
          }
      where
        theta :: State -> State
        theta s = fromJust (findIndex (S.member s) p)

dfaToNFA :: DFA -> N.NFA
dfaToNFA d =
  N.NFA 
   { N.alphabet         = alphabet d
   , N.states           = states d
   , N.accepting_states = accepting_states d
   , N.transitions      = S.map (\(f,l,t) -> (f,l,S.singleton t)) (transitions d)
   , N.start            = start d
   , N.failure          = failure d
   }

nfaToDFA :: N.NFA -> DFA
nfaToDFA n = 
 DFA
  { alphabet         = N.alphabet n
  , states           = S.map theta combined_states
  , accepting_states = S.map theta (S.filter (any (\x -> S.member x (N.accepting_states n))) combined_states)
  , transitions      = S.map (\(f,l,t) -> (theta f,l,theta t)) $ S.foldr S.union S.empty $ S.map aggregated_transitions combined_states
  , start            = theta (S.singleton (N.start n))
  , failure          = theta (S.singleton (N.failure n))
  }  
  where
   theta :: S.Set State -> State
   theta s = fromJust (elemIndex s (S.toList combined_states))

   combined_states :: S.Set (S.Set State)
   combined_states = go [S.singleton (N.start n)] S.empty

   go :: [S.Set State] -> S.Set (S.Set State) -> S.Set (S.Set State)
   go []    cs = cs
   go (s:w) cs
     | S.member s cs  = go w cs
     | otherwise      = 
         let w' = map (\c -> S.foldr S.union S.empty $ S.map (\(_,_,t) -> t) $ S.filter (\(f,l,_) -> l == c && S.member f s) $ N.transitions n) (S.toList (N.alphabet n))
         in go (w' ++ w) (S.insert s cs)

   aggregated_transitions :: S.Set State -> S.Set (S.Set State,Label,S.Set State)
   aggregated_transitions s = 
     S.map (\c -> (s,c,S.foldr S.union S.empty $ S.map (\(_,_,t) -> t) $ S.filter (\(f,l,_) -> l == c && S.member f s) $ N.transitions n)) 
           (N.alphabet n)
   
sequence :: DFA -> DFA -> DFA
sequence d1 d2 = minimize (nfaToDFA (N.sequence (dfaToNFA (minimize d1)) (dfaToNFA (minimize d2))))


toDot :: DFA -> Dot ()
toDot d =
  do tuples <- mapM mkNode (S.toList $ states d)
     let m = M.fromList tuples
     mapM_ (\(f,l,t) -> edge (m M.! f) (m M.! t) [("label",show l)])(transitions d)
     dummy <- node [("shape","none"),("style","invisible")]
     edge dummy (m M.! start d) []
     return ()
 where
   mkNode n = 
     do i <- node [("shape",shape)]
        return (n,i)
    where
      shape 
       | S.member n (accepting_states d)
       = "doublecircle"
       | otherwise 
       = "circle"


toString :: DFA -> String
toString = showDot . toDot
