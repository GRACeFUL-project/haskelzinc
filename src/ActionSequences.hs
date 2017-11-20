module ActionSequences where
  
import DFA
import qualified Data.Set as S
import Data.Maybe (fromJust)

import Interfaces.MZinHaskell
import Interfaces.MZAST
import Interfaces.FZSolutionParser (Solution)
import Text.Parsec.Error

type Label = Int
type State = Int

-- | An action sequence model
data ASModel = ASModel Int      -- ^ The number of actions
                       [ASExpr] -- ^ A list of expressions
  deriving (Show)

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
  deriving (Show)

-- | Constructors
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

-- | Transform an action sequence model into a DFA
asModelToImplDFA :: ASModel -> ImplDFA
asModelToImplDFA (ASModel k es) = foldl DFA.sequence (emptyImplDFA k) $
  map (dfaToImplDFA . (asExprToDFA k)) es

-- | Transform an action sequence expression into a DFA
--
-- * k = the number of actions
-- * e = the action sequence expression
asExprToDFA :: Int -> ASExpr -> DFA
asExprToDFA k e = case e of
  Atleast i p         -> constr_atLeast k i p
  Atmost i p          -> constr_atMost k i p
  Incompatible i j    -> constr_incompatible k i j
  Implication i j     -> constr_implication k i j
  ValuePrecedence i j -> constr_value_precedence k i j
  StretchMin i s      -> constr_stretch_min k i s
  StretchMax i s      -> constr_stretch_max k i s
  Or i j              -> constr_or k i j

-- | An empty Implicit DFA
--
-- * k = the number of actions
emptyImplDFA :: Int -> ImplDFA
emptyImplDFA k =
  ImplDFA
   { alphabetI         = S.fromList abc
   , statesI           = S.fromList (failure : padding : [1])
   , accepting_statesI = S.fromList (padding : [1])
   , transitionsI      =           S.fromList ((1,nop,padding) : [(1,a,1) | a <- (next : [1..k])])
                        `S.union` S.fromList ((padding,nop,padding) : [(padding,a,failure) | a <- (next : [1..k])])
   , startI = 1
   }
   where
     abc = [1..k+2]

     next = k + 1
     nop  = k + 2

     failure  = 0
     padding  = 2

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
    q  = S.size (statesI atm)
    s  = S.size (alphabetI atm)
    d  = [[ transitionI atm state label | label <- [1..s] ] | state <- [1..q]]
    q0 = startI atm
    f  = S.toList (accepting_statesI atm)

test1 :: String
test1 = layout [constraint (dfaToRegular (dfaToImplDFA (or_ctr 2 1 2)) (Var (Simpl "xs")))]

-- | Converts a DFA to a HaskellZinc model
--
-- * s = The variable name
-- * d = The given DFA
dfaToModel :: String -> ImplDFA -> [ModelData]
dfaToModel s d = [constraint (dfaToRegular d (Var (Simpl s)))]

runActionSeqModel :: ASModel -- ^ The model
  -> String                  -- ^ The variable name
  -> FilePath                -- ^ The path where the FlatZinc translation will be printed
  -> Int                     -- ^ The chosen solver
  -> Int                     -- ^ The number of solutions to be returned
  -> IO (Either ParseError [Solution])
runActionSeqModel m n p s ns =
  let model = dfaToModel n $ asModelToImplDFA m
  in runModel model p s ns

main :: IO ()
-- main = render $ or_ctr 2 1 2
main = putStrLn test1
