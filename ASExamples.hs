module ASExamples where

import Interfaces.MZBuiltIns
import Interfaces.MZAST
import Interfaces.MZinHaskell

main = iRunModel example1

example1 = [
  include "regular.mzn",
  var (Array [CT $ 1..20] Dec (CT $ 1..4)) "x",
  actionSequence 2 (atmost 1 3) (Var (Simpl "x"))
  solve satisfy
           ]
