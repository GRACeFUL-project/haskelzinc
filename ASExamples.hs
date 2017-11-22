module ASExamples where

import Interfaces.MZBuiltIns
import Interfaces.MZinHaskell
import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences
import TimeSpaceConstr.TimeSpaceConstr

main = iRunModel example1

example1 = [
  include "regular.mzn",
  var (Array [CT $ 1...20] Dec (CT $ 1...4)) "x",
  constraint $ actionSequence 2 (atmost 1 3) (Var (Simpl "x")),
  solve satisfy
           ]
