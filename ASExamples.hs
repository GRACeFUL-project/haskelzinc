{-# LANGUAGE OverloadedStrings #-}

module ASExamples where

import Interfaces.MZBuiltIns
import Interfaces.MZinHaskell
import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences
import TimeSpaceConstr.TimeSpaceConstr

main = iRunModel example2

example1 = [ include "regular.mzn"
           , var (Array [CT $ 1...20] Dec (CT $ 1...4)) "x"
           , actionSequence 2 "x" (atmost 1 3)
           , solve satisfy
           ]

example2 = (useCostPreds [uniformCostPred, discountCostPred, dependentCostPred])
        ++ [ include "regular.mzn"
           , var (Array [CT $ 1...10] Dec (CT $ 1...8)) "x"
           , actionSequence 6 "x" (atleast_cells 2)
           , actionSequence 6 "x" (atleast 1 1)
           , actionSequence 6 "x" (or_as 2 3)
           , actionSequence 6 "x" (or_as 3 4)
           , actionSequence 6 "x" (or_as 5 6)
           , actionSequence 6 "x" (implication 2 6)
           , actionSequence 6 "x" (value_precedence 3 5)
           , var Int "c1", var Int "c2", var Int "c3"
           , var Int "c4", var Int "c5", var Int "c6"
           , actionSequenceCost "x" "c1" (uniformCost 1 5)
           , actionSequenceCost "x" "c2" (uniformCost 2 3)
           , actionSequenceCost "x" "c3" (uniformCost 3 7)
           , actionSequenceCost "x" "c4" (discountCost 4 7 5 3 False)
           , actionSequenceCost "x" "c5" (discountCost 5 7 10 6 False)
           , actionSequenceCost "x" "c6" (dependentCost 6 3 7 8 4 False)
           , var Int "cost" =. "c1" +. "c2" +. "c3" +. "c4" +. "c5" +. "c6"
           , solve satisfy
           ]
