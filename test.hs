  {- 
      === INSTRUCTIONS ===
    Below are provided some models in the Haskell abstract syntax tree for MiniZinc,
    together with their data files (for the models that need one).
    
      == Models without a datafile ==
    The command to interactively run a model is
      > iTestModel <model_name>
    Then, follow the script's instructions.
    
      == Models with datafile ==
    If the model needs to be given a data file, then you have to first 
    run the command
      > writeData <data_name>
    The script will ask you for the desired path of the .dzn file.
    
    You should use
      > testModelWithData <model_name> <data_name> <filepath> <solver> <num_of_solutions>
    to run a model with a data file. This command is not interactive.
  -}

module HaskelzincTests where

import Interfaces.MZinHaskell
  
unsatisfiable =[
  Declare (Dec, (Range (IConst 1) (IConst 3))) "k" Nothing,
  Constraint $ Bi Gt (Var "k") (IConst 3),
  Solve Satisfy
  ]

planning = [
  Declare (Par, Int) "nproducts" Nothing,
  Declare (Par, Set Int) "Products" (Just (Interval (IConst 1) (Var "nproducts"))),
  Declare (Par, Array [AOS "Products"] (Par, Int)) "profit" Nothing,
  Declare (Par, Array [AOS "Products"] (Par, String)) "pname" Nothing,
  Declare (Par, Int) "nresources" Nothing,
  Declare (Par, Set Int) "Resources" (Just (Interval (IConst 1) (Var "nresources"))),
  Declare (Par, Array [AOS "Resources"] (Par, Int)) "capacity" Nothing,
  Declare (Par, Array [AOS "Resources"] (Par, String)) "rname" Nothing,
  Declare (Dec, Array [AOS "Products", AOS "Resources"] (Par, Int)) "consumption" Nothing,
  Constraint (Call mz_assert [Call mz_forall [ArrayComp (Bi Gte (ArrayElem "consumption" [Var "p", Var "r"]) (IConst 0)) ([(["r"], Var "Resources"), (["p"], Var "Products")], Nothing)], SConst "Error: Negative consumption"]),
  Declare (Par, Int) "mproducts" (Just (Call mz_max [ArrayComp (Call mz_min [ArrayComp (Bi IDiv (ArrayElem "capacity" [Var "r"]) (ArrayElem "consumption" [Var "p", Var "r"])) ([(["r"], Var "Resources")], (Just (Bi Gt (ArrayElem "consumption" [Var "p", Var "r"]) (IConst 0))))]) ([(["p"], Var "Products")], Nothing)])),
  Declare (Dec, Array [AOS "Products"] (Dec, Range (IConst 0) (Var "mproducts"))) "produce" Nothing,
  Declare (Dec, Array [AOS "Resources"] (Dec, Range (IConst 0) (Call mz_max [Var "capacity"]))) "used" Nothing,
  Constraint (Call mz_forall [ArrayComp (Bi And (Bi Eq (ArrayElem "used" [Var "r"]) (Call mz_sum [ArrayComp (Bi Times (ArrayElem "consumption" [Var "p", Var "r"]) (ArrayElem "produce" [Var "p"])) ([(["p"],Var "Products")], Nothing)])) (Bi Lte (ArrayElem "used" [Var "r"]) (ArrayElem "capacity" [Var "r"]))) ([(["r"], Var "Resources")], Nothing)]),
  Solve $ Maximize (Call mz_sum [ArrayComp (Bi Times (ArrayElem "profit" [Var "p"]) (ArrayElem "produce" [Var "p"])) ([(["p"],Var "Products")], Nothing)]),
  Output (Bi Concat (ArrayComp (Bi Concat (Bi Concat (Bi Concat (Call mz_show [ArrayElem "pname" [Var "p"]]) (SConst " = ")) (Call mz_show [ArrayElem "produce" [Var "p"]])) (SConst "\n")) ([(["p"], Var "Products")], Nothing)) (ArrayComp (Bi Concat (Bi Concat (Bi Concat (Call mz_show [ArrayElem "rname" [Var "r"]]) (SConst " = ")) (Call mz_show [ArrayElem "used" [Var "r"]])) (SConst "\n")) ([(["r"], Var "Resources")], Nothing)))]

planningData = [
  Assign "nproducts" (IConst 2),
  Assign "profit" (ArrayLit [IConst 400, IConst 450]),
  Assign "pname" (ArrayLit [SConst "banana-cake", SConst "chocolate-cake"]),
  Assign "nresources" (IConst 5),
  Assign "capacity" (ArrayLit [IConst 4000, IConst 6, IConst 2000, IConst 500, IConst 500]),
  Assign "rname" (ArrayLit [SConst "flour", SConst "banana", SConst "sugar", SConst "butter", SConst "cocoa"]),
  Assign "consumption" (ArrayLit2D [[IConst 250, IConst 2, IConst 75, IConst 100, IConst 0], [IConst 200, IConst 0, IConst 150, IConst 150, IConst 75]])]

knapsack = [
  Declare (Par, Int) "n" Nothing,
  Declare (Par, Set Int) "Items" (Just (Interval (IConst 1) (Var "n"))),
  Declare (Par, Int) "capacity" Nothing,
  Empty,
  Declare (Par, Array [AOS "Items"] (Par, Int)) "profits" Nothing,
  Declare (Par, Array [AOS "Items"] (Par, Int)) "weights" Nothing,
  Empty,
  Declare (Dec, Set (AOS "Items")) "knapsack" Nothing,
  Empty,
  Constraint $ Bi Lte (GenCall mz_sum ([(["i"], Var "Items")], Nothing) (Bi Times (Call mz_bool2int [Bi In (Var "i") (Var "knapsack")]) (ArrayElem "weights" [Var "i"]))) (Var "capacity"),
  Empty,
  Solve $ Maximize (GenCall mz_sum ([(["i"], Var "Items")], Nothing) (Bi Times (Call mz_bool2int [Bi In (Var "i") (Var "knapsack")]) (ArrayElem "profits" [Var "i"])))]

knapdata = [
  Assign "n" (IConst 6),
  Assign "capacity" (IConst 13),
  Assign "profits" $ ArrayLit [IConst 5, IConst 9, IConst 15, IConst 10, IConst 3, IConst 6],
  Assign "weights" (Interval (IConst 1) (IConst 6))]

australia = [
  Comment "Colouring Australia using nc colours",
  Declare (Par, Int) "nc" (Just (IConst 3)),
  Declare (Dec, Range (IConst 1) (Var "nc")) "wa" Nothing,
  Declare (Dec, Range (IConst 1) (Var "nc")) "nsw" Nothing,
  Declare (Dec, Range (IConst 1) (Var "nc")) "nt" Nothing,
  Declare (Dec, Range (IConst 1) (Var "nc")) "v" Nothing,
  Declare (Dec, Range (IConst 1) (Var "nc")) "sa" Nothing,
  Declare (Dec, Range (IConst 1) (Var "nc")) "t" Nothing,
  Declare (Dec, Range (IConst 1) (Var "nc")) "q" Nothing,
  Constraint (Bi Neq (Var "wa") (Var "nt")),
  Constraint (Bi Neq (Var "wa") (Var "sa")),
  Constraint (Bi Neq (Var "nt") (Var "sa")),
  Constraint (Bi Neq (Var "nt") (Var "q")),
  Constraint (Bi Neq (Var "sa") (Var "q")),
  Constraint (Bi Neq (Var "sa") (Var "nsw")),
  Constraint (Bi Neq (Var "sa") (Var "v")),
  Constraint (Bi Neq (Var "q") (Var "nsw")),
  Constraint (Bi Neq (Var "nsw") (Var "v")),
  Solve Satisfy,
  Output (ArrayLit [
    SConst "wa=",
    Call mz_show [Var "wa"],
    SConst "\t nt=",
    Call mz_show [Var "nt"],
    SConst "\t sa=",
    Call mz_show [Var "sa"],
    SConst "\n",
    SConst "q=",
    Call mz_show [Var "q"],
    SConst "\t nsw=",
    Call mz_show [Var "nsw"],
    SConst "\t v=",
    Call mz_show [Var "v"],
    SConst "\n",
    SConst "t=",
    Call mz_show [Var "t"],
    SConst "\n"])]

cakes = [
  Comment "Baking cakes for the school fete (with data file)",
  Declare (Par, Int) "flour" Nothing,
  Declare (Par, Int) "banana" Nothing,
  Declare (Par, Int) "sugar" Nothing,
  Declare (Par, Int) "butter" Nothing,
  Declare (Par, Int) "cocoa" Nothing,
  Constraint (Call mz_assert [Bi Gte (Var "flour") (IConst 0), Bi Concat (SConst "Invalid datafile: ") (SConst "Ammount of flour is non-negative")]),
  Constraint (Call mz_assert [Bi Gte (Var "banana") (IConst 0), Bi Concat (SConst "Invalid datafile: ") (SConst "Ammount of banana is non-negative")]),
  Constraint (Call mz_assert [Bi Gte (Var "sugar") (IConst 0), Bi Concat (SConst "Invalid datafile: ") (SConst "Ammount of sugar is non-negative")]),
  Constraint (Call mz_assert [Bi Gte (Var "butter") (IConst 0), Bi Concat (SConst "Invalid datafile: ") (SConst "Ammount of butter is non-negative")]),
  Constraint (Call mz_assert [Bi Gte (Var "cocoa") (IConst 0), Bi Concat (SConst "Invalid datafile: ") (SConst "Ammount of cocoa is non-negative")]),
  Declare (Dec, Range (IConst 0) (IConst 100)) "b" Nothing,
  Declare (Dec, Range (IConst 0) (IConst 100)) "c" Nothing,
  Constraint (Bi Lte (Bi BPlus (Bi Times (IConst 250) (Var "b")) (Bi Times (IConst 200) (Var "c"))) (Var "flour")),
  Constraint (Bi Lte (Bi Times (IConst 2) (Var "b")) (Var "banana")),
  Constraint (Bi Lte (Bi BPlus (Bi Times (IConst 75) (Var "b")) (Bi Times (IConst 150) (Var "c"))) (Var "sugar")),
  Constraint (Bi Lte (Bi BPlus (Bi Times (IConst 100) (Var "b")) (Bi Times (IConst 150) (Var "c"))) (Var "butter")),
  Constraint (Bi Lte (Bi Times (IConst 75) (Var "c")) (Var "cocoa")),
  Comment "Maximize our profit",
  Solve (Maximize (Bi BPlus (Bi Times (IConst 400) (Var "b")) (Bi Times (IConst 450) (Var "c")))),
  Output (ArrayLit [
    SConst "no. of banana cakes = ", Call mz_show [Var "b"], SConst "\n", SConst "no. of chocolate cakes = ", Call mz_show [Var "c"], SConst "\n"
  ])]
    
cakedata = [
  Assign "flour" (IConst 4000),
  Assign "banana" (IConst 6),
  Assign "sugar" (IConst 2000),
  Assign "butter" (IConst 500),
  Assign "cocoa" (IConst 500)]
{-
test1 = printExpr $ SetComp (Bi Times (IConst 2) (Var "i")) ([(["i"], Interval (IConst 1) (IConst 5))], Nothing)
-- results to: {2 * i | i in 1..5}

test2 = printExpr $ SetComp (Bi BPlus (Bi Times (IConst 3) (Var "i")) (Var "j")) ([(["i"], Interval (IConst 0) (IConst 2)), (["j"], SetLit [IConst 0, IConst 1])], Nothing)
-- results to: {3 * i + j | i in 0..2, j in {0, 1}}

test3 = printExpr $ Call Forall [ArrayComp (Bi Neq (ArrayElem "a" [Var "i"]) (ArrayElem "a" [Var "j"])) ([(["i", "j"], Interval (IConst 1) (IConst 3))], Nothing)]
-- results to: forall([a[i] != a[j] | i, j in 1..3])

test4 = printExpr $ GenCall Forall ([(["i", "j"], Interval (IConst 1) (IConst 3))], Nothing) (Bi Neq (ArrayElem "a" [Var "i"]) (ArrayElem "a" [Var "j"]))
-- results to:  forall(i, j in 1..3)
--                (a[i] != a[j])

test5 = printExpr $ ArrayComp (Call (UserD "f") [Var "i", Var "j"]) ([(["i"], ArrayComp (Var "k") ([(["k"], Interval (IConst 0) (IConst 100))], Just (Bi Eqq (Bi Mod (Var "k") (IConst 7)) (IConst 2)))), (["j"], SetLit [IConst 0, IConst 1])], Nothing)
-- results to: [f(i, j) | i in [k | k in 0..100 where k mod 7 == 2], j in {0, 1}]

test6 = printExpr $ ArrayLit2D [[IConst 1, IConst 2, IConst 3],[IConst 4, IConst 5, IConst 6],[IConst 7, IConst 8, IConst 9]]
-- results to: [| 1, 2, 3
--              | 4, 5, 6
--              | 7, 8, 9|]

test7 = printExpr $ ITE [(Bi Lte (Var "x") (Var "y"), Var "x")] (Var "y")
-- results to: if x <= y then x
--             else y endif

test8 = printExpr $ ITE [(Bi Lt (Var "x") (IConst 0), U UMinus (IConst 1)), (Bi Gt (Var "x") (IConst 0), IConst 1)] (IConst 0)
-- results to: if x < 0 then - 1
--             elseif x > 0 then 1
--             else 0 endif

test9 = printExpr $ Let [Declare Dec Int "x" (Just (IConst 3)), Declare Dec Int "y" (Just (IConst 4))] (Bi BPlus (Var "x") (Var "y"))
-- results to: let {var int: x = 3;
--                  var int: y = 4;}
--             in x + y

test10 = printItem $ Pred "even" [(Dec, Int, "x")] (Just (Bi Eq (Bi Mod (Var "x") (IConst 2)) (IConst 0)))
-- results to: predicate even(var int: x) =
--               x mod 2 = 0;
-}