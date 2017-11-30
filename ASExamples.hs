module ASExamples where

import Interfaces.MZBuiltIns
import Interfaces.MZinHaskell
import Interfaces.MZAST
import TimeSpaceConstr.ActionSequences
import TimeSpaceConstr.TimeSpaceConstr

main = iRunModel example2

example1 = [
  include "regular.mzn",
  var (Array [CT $ 1...20] Dec (CT $ 1...4)) "x",
  actionSequence 2 "x" (atmost 1 3),
  solve satisfy
           ]

example2 = [
  include "regular.mzn",
  var (Array [CT $ 1...10] Dec (CT $ 1...8)) "x",
  actionSequence 6 "x" (atleast 1 1),
  actionSequence 6 "x" (or_as 2 3),
  actionSequence 6 "x" (or_as 3 4),
  actionSequence 6 "x" (or_as 5 6),
  actionSequence 6 "x" (implication 2 6),
  actionSequence 6 "x" (value_precedence 3 5),
  solve satisfy
           ]
