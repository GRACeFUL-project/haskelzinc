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
  actionSequence 2 (atmost 1 3) (Var (Simpl "x")),
  solve satisfy
           ]

example2 = [
  include "regular.mzn",
  var (Array [CT $ 1...10] Dec (CT $ 1...8)) "x",
  actionSequence 6 (atleast 1 1) (Var (Simpl "x")),
  actionSequence 6 (or_as 2 3) (Var (Simpl "x")),
  actionSequence 6 (or_as 3 4) (Var (Simpl "x")),
  actionSequence 6 (or_as 5 6) (Var (Simpl "x")),
  actionSequence 6 (implication 2 6) (Var (Simpl "x")),
  actionSequence 6 (value_precedence 3 5) (Var (Simpl "x")),
  solve satisfy
           ]
