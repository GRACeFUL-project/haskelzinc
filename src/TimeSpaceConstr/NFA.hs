module TimeSpaceConstr.NFA
 ( NFA (..)
 , sequence
 ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (findIndex,elemIndex,delete)
import Prelude hiding (sequence)

type Label = Int
type State = Int

data NFA =
  NFA { alphabet         :: S.Set Label
      , states           :: S.Set State
      , accepting_states :: S.Set State
      , transitions      :: S.Set (State,Label,S.Set State)
      , start            :: State
      } deriving Show

sequence :: NFA -> NFA -> NFA
sequence n1 n2 =
  NFA
   { alphabet         = alphabet n1 
   , states           = S.union (S.map theta1 (states n1)) (S.map theta2 (states n2))
   , accepting_states = S.map theta2 (accepting_states n2)
   , transitions      = S.union 
                            (S.map (\(f,l,t) -> let t' = if (any (\s -> S.member s (accepting_states n1)) t) 
                                                           then theta2 (start n2) `S.insert` S.map theta1 t
                                                           else S.map theta1 t
                                                in (theta1 f,l,t')) 
                                   (transitions n1))
                            (S.map (\(f,l,t) -> (theta2 f,l,S.map theta2 t)) (transitions n2))
   , start            = theta1 (start n1)
   }
  where
   theta1 :: State -> State
   theta1 s = fromJust (elemIndex s (S.toList (states n1))) 
   theta2 :: State -> State
   theta2 s = fromJust (elemIndex s (S.toList (states n2))) + S.size (states n1)
