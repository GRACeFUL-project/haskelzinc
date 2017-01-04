{-# LANGUAGE ExtendedDefaultRules #-}
  {- 
      === INSTRUCTIONS ===
    Below are provided some models in the Haskell abstract syntax tree for MiniZinc,
    together with their data files (for the models that need one).
    
      == Models without a datafile ==
    The command to interactively run a model is
      > iTestModel <model_name>
    Then, follow the script's instructions.
    
      == Models with datafile ==    
    You should use
      > testModelWithData <model_name> <data_name> <filepath> <solver> <num_of_solutions>
    to run a model with data. This command is not interactive.
    
    To write the model's data into a .dzn file, use 
      > writeData <data_name>
    The script will ask you for the desired path of the file.
  -}

module HaskelzincTests where

import Interfaces.MZBuiltIns
import Interfaces.MZinHaskell
import Interfaces.MZASTBase
import Interfaces.MZAST

default (Int, Double)

small = Call $ mz_abs [Expr (IConst 3) [], Expr (IConst 5) []]
big   =
  Call $ mz_discrete_distribution [Expr (Call $ mz_dom_bounds_array [Expr (Var "somebigarrayname") []]) []
                                  ,Expr (Bi mz_intersect (Var "areallybigset") (Var "coulditneedmore?")) []]

{-  
unsatisfiable =[
  Declare $ Declaration (Variable (Dec, (Range (IConst 1) (IConst 3)), "k")) [] Nothing,
  --var (Dec, (Range (IConst 1) (IConst 3))) "k",
  Constraint $ Expr (Bi mz_gt (Var "k") (IConst 3)) [],
  Solve [] Satisfy
  ]
-}
planning = [
  Declare $ Declaration (Variable (Par, Int, "nproducts")) [] Nothing,
  Declare $ Declaration (Variable (Par, Set Int, "Products")) [] (Just (Expr (Bi mz_range (IConst 1) (Var "nproducts")) [])),
  Declare $ Declaration (Variable (Par, Array [AOS "Products"] (Par, Int), "profit")) [] Nothing,
  Declare $ Declaration (Variable (Par, Array [AOS "Products"] (Par, String), "pname")) [] Nothing,
  Declare $ Declaration (Variable (Par, Int, "nresources")) [] Nothing,
  Declare $ Declaration (Variable (Par, Set Int, "Resources")) [] (Just (Expr (Bi mz_range (IConst 1) (Var "nresources")) [])),
  Declare $ Declaration (Variable (Par, Array [AOS "Resources"] (Par, Int), "capacity")) [] Nothing,
  Declare $ Declaration (Variable (Par, Array [AOS "Resources"] (Par, String), "rname" )) [] Nothing,
  Declare $ Declaration (Variable (Dec, Array [AOS "Products", AOS "Resources"] (Par, Int), "consumption")) [] Nothing,
  Constraint $
    Expr (Call $ mz_assert [Expr (Call $ mz_forall [Expr (ArrayComp (Bi mz_gte (ArrayElem "consumption" [Var "p", Var "r"]) (IConst 0)) 
                                                                    ([(["r"], Var "Resources"), (["p"], Var "Products")], Nothing)) []]) []
                           , Expr (SConst "Error: Negative consumption") []]) [],
  Declare $
    Declaration
      (Variable (Par, Int, "mproducts")) [] 
      (Just (Expr (Call $ mz_max [Expr (ArrayComp (Call $ mz_min [Expr (ArrayComp (Bi mz_idiv (ArrayElem "capacity" [Var "r"]) (ArrayElem "consumption" [Var "p", Var "r"])) ([(["r"], Var "Resources")], (Just (Bi mz_gt (ArrayElem "consumption" [Var "p", Var "r"]) (IConst 0))))) []]) ([(["p"], Var "Products")], Nothing)) []]) [])),
  Declare $ Declaration (Variable (Dec, Array [AOS "Products"] (Dec, Range (IConst 0) (Var "mproducts")), "produce")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Array [AOS "Resources"] (Dec, Range (IConst 0) (Call $ mz_max [Expr (Var "capacity") []])), "used")) [] Nothing,
  Constraint $ Expr (Call $ mz_forall [Expr (ArrayComp (Bi mz_and (Bi mz_eq (ArrayElem "used" [Var "r"]) (Call $ mz_sum [Expr (ArrayComp (Bi mz_times (ArrayElem "consumption" [Var "p", Var "r"]) (ArrayElem "produce" [Var "p"])) ([(["p"],Var "Products")], Nothing)) []])) (Bi mz_lte (ArrayElem "used" [Var "r"]) (ArrayElem "capacity" [Var "r"]))) ([(["r"], Var "Resources")], Nothing)) []]) [],
  Solve [] (Maximize $ Expr (Call $ mz_sum [Expr (ArrayComp (Bi mz_times (ArrayElem "profit" [Var "p"]) (ArrayElem "produce" [Var "p"])) ([(["p"],Var "Products")], Nothing)) []]) [])
  ]

planningData = [
  Assign "nproducts" $ Expr (IConst 2) [],
  Assign "profit" $ Expr (ArrayLit [IConst 400, IConst 450]) [],
  Assign "pname" $ Expr (ArrayLit [SConst "banana-cake", SConst "chocolate-cake"]) [],
  Assign "nresources" $ Expr (IConst 5) [],
  Assign "capacity" $ Expr (ArrayLit [IConst 4000, IConst 6, IConst 2000, IConst 500, IConst 500]) [],
  Assign "rname" $ Expr (ArrayLit [SConst "flour", SConst "banana", SConst "sugar", SConst "butter", SConst "cocoa"]) [],
  Assign "consumption" $ Expr (ArrayLit2D [[IConst 250, IConst 2, IConst 75, IConst 100, IConst 0], [IConst 200, IConst 0, IConst 150, IConst 150, IConst 75]]) []]

knapsack = [
  Declare $ Declaration (Variable (Par, Int, "n")) [] Nothing,
  Declare $ Declaration (Variable (Par, Set Int, "Items")) [] (Just (Expr (Bi mz_range (IConst 1) (Var "n")) [])),
  Declare $ Declaration (Variable (Par, Int, "capacity")) [] Nothing,
  Empty,
  Declare $ Declaration (Variable (Par, Array [AOS "Items"] (Par, Int), "profits")) [] Nothing,
  Declare $ Declaration (Variable (Par, Array [AOS "Items"] (Par, Int), "weights")) [] Nothing,
  Empty,
  Declare $ Declaration (Variable (Dec, Set (AOS "Items"), "knapsack")) [] Nothing,
  Empty,
  Constraint $ Expr (Bi mz_lte (GenCall "sum" ([(["i"], Var "Items")], Nothing) (Bi mz_times (Call $ mz_bool2int [Expr (Bi mz_in (Var "i") (Var "knapsack")) []]) (ArrayElem "weights" [Var "i"]))) (Var "capacity")) [],
  Empty,
  Solve [] $ Maximize (Expr (GenCall "sum" ([(["i"], Var "Items")], Nothing) (Bi mz_times (Call $ mz_bool2int [Expr (Bi mz_in (Var "i") (Var "knapsack")) []]) (ArrayElem "profits" [Var "i"]))) [])]

knapdata = [
  Assign "n" $ Expr (IConst 6) [],
  Assign "capacity" $ Expr (IConst 13) [],
  Assign "profits" $ Expr (ArrayLit [IConst 5, IConst 9, IConst 15, IConst 10, IConst 3, IConst 6]) [],
  Assign "weights" $ Expr (Bi mz_range (IConst 1) (IConst 6)) []]

australia = [
  Comment "Colouring Australia using nc colours",
  Declare $ Declaration (Variable (Par, Int, "nc")) [] (Just (Expr (IConst 3) [])),
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "wa")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "nsw")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "nt")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "v")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "sa")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "t")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 1) (Var "nc"), "q")) [] Nothing,
  Constraint $ Expr (Bi mz_neq (Var "wa") (Var "nt")) [],
  Constraint $ Expr (Bi mz_neq (Var "wa") (Var "sa")) [],
  Constraint $ Expr (Bi mz_neq (Var "nt") (Var "sa")) [],
  Constraint $ Expr (Bi mz_neq (Var "nt") (Var "q")) [],
  Constraint $ Expr (Bi mz_neq (Var "sa") (Var "q")) [],
  Constraint $ Expr (Bi mz_neq (Var "sa") (Var "nsw")) [],
  Constraint $ Expr (Bi mz_neq (Var "sa") (Var "v")) [],
  Constraint $ Expr (Bi mz_neq (Var "q") (Var "nsw")) [],
  Constraint $ Expr (Bi mz_neq (Var "nsw") (Var "v")) [],
  Solve [] Satisfy,
  Output (ArrayLit [
    SConst "wa=",
    Call $ mz_show [Expr (Var "wa") []],
    SConst "\t nt=",
    Call $ mz_show [Expr (Var "nt") []],
    SConst "\t sa=",
    Call $ mz_show [Expr (Var "sa") []],
    SConst "\n",
    SConst "q=",
    Call $ mz_show [Expr (Var "q") []],
    SConst "\t nsw=",
    Call $ mz_show [Expr (Var "nsw") []],
    SConst "\t v=",
    Call $ mz_show [expr $ Var "v"],
    SConst "\n",
    SConst "t=",
    Call $ mz_show [Expr (Var "t") []],
    SConst "\n"])]

cakes = [
  Comment "Baking cakes for the school fete (with data file)",
  Declare $ Declaration (Variable (Par, Int, "flour")) [] Nothing,
  Declare $ Declaration (Variable (Par, Int, "banana")) [] Nothing,
  Declare $ Declaration (Variable (Par, Int, "sugar")) [] Nothing,
  Declare $ Declaration (Variable (Par, Int, "butter")) [] Nothing,
  Declare $ Declaration (Variable (Par, Int, "cocoa")) [] Nothing,
  Constraint $ 
    Expr (Call $ mz_assert [Expr (Bi mz_gte (Var "flour") (IConst 0)) []
                           ,Expr (Bi mz_pp (SConst "mz_invalid datafile: ") (SConst "Ammount of flour is non-negative")) []]
         ) [],
  Constraint $
    Expr (Call $ mz_assert [Expr (Bi mz_gte (Var "banana") (IConst 0)) []
                           ,Expr (Bi mz_pp (SConst "mz_invalid datafile: ") (SConst "Ammount of banana is non-negative")) []]
         ) [],
  Constraint $
    Expr (Call $ mz_assert [Expr (Bi mz_gte (Var "sugar") (IConst 0)) []
                           ,Expr (Bi mz_pp (SConst "mz_invalid datafile: ") (SConst "Ammount of sugar is non-negative")) []]
         ) [],
  Constraint $
    Expr (Call $ mz_assert [Expr (Bi mz_gte (Var "butter") (IConst 0)) []
                           ,Expr (Bi mz_pp (SConst "mz_invalid datafile: ") (SConst "Ammount of butter is non-negative")) []]
         ) [],
  Constraint $
    Expr (Call $ mz_assert [Expr (Bi mz_gte (Var "cocoa") (IConst 0)) []
                           ,Expr (Bi mz_pp (SConst "mz_invalid datafile: ") (SConst "Ammount of cocoa is non-negative")) []]
         ) [],
  Declare $ Declaration (Variable (Dec, Range (IConst 0) (IConst 100), "b")) [] Nothing,
  Declare $ Declaration (Variable (Dec, Range (IConst 0) (IConst 100), "c")) [] Nothing,
  Constraint $ Expr (Bi mz_lte (Bi mz_plus (Bi mz_times (IConst 250) (Var "b")) (Bi mz_times (IConst 200) (Var "c"))) (Var "flour")) [],
  Constraint $ Expr (Bi mz_lte (Bi mz_times (IConst 2) (Var "b")) (Var "banana")) [],
  Constraint $ Expr (Bi mz_lte (Bi mz_plus (Bi mz_times (IConst 75) (Var "b")) (Bi mz_times (IConst 150) (Var "c"))) (Var "sugar")) [],
  Constraint $ Expr (Bi mz_lte (Bi mz_plus (Bi mz_times (IConst 100) (Var "b")) (Bi mz_times (IConst 150) (Var "c"))) (Var "butter")) [],
  Constraint $ Expr (Bi mz_lte (Bi mz_times (IConst 75) (Var "c")) (Var "cocoa")) [],
  Comment "Maximize our profit",
  Solve [] (Maximize (Expr (Bi mz_plus (Bi mz_times (IConst 400) (Var "b")) (Bi mz_times (IConst 450) (Var "c"))) [])),
  Output (ArrayLit [
    SConst "no. of banana cakes = ", Call $ mz_show [Expr (Var "b") []], SConst "\n", SConst "no. of chocolate cakes = ", Call $ mz_show [Expr (Var "c") []], SConst "\n"
  ])]
    
cakedata = [
  Assign "flour"  $ Expr (IConst 4000) [],
  Assign "banana" $ Expr (IConst 6)    [],
  Assign "sugar"  $ Expr (IConst 2000) [],
  Assign "butter" $ Expr (IConst 500)  [],
  Assign "cocoa"  $ Expr (IConst 500)  []]
{-
test1 = printExpr $ SetComp (Bi mz_times (IConst 2) (Var "i")) ([(["i"], mz_interval (IConst 1) (IConst 5))], Nothing)
-- results to: {2 * i | i in 1..5}

test2 = printExpr $ SetComp (Bi mz_plus (Bi mz_times (IConst 3) (Var "i")) (Var "j")) ([(["i"], mz_interval (IConst 0) (IConst 2)), (["j"], SetLit [IConst 0, IConst 1])], Nothing)
-- results to: {3 * i + j | i in 0..2, j in {0, 1}}

test3 = printExpr $ Call Forall [ArrayComp (Bi mz_neq (ArrayElem "a" [Var "i"]) (ArrayElem "a" [Var "j"])) ([(["i", "j"], mz_interval (IConst 1) (IConst 3))], Nothing)]
-- results to: forall([a[i] != a[j] | i, j in 1..3])

test4 = printExpr $ GenCall Forall ([(["i", "j"], mz_interval (IConst 1) (IConst 3))], Nothing) (Bi mz_neq (ArrayElem "a" [Var "i"]) (ArrayElem "a" [Var "j"]))
-- results to:  forall(i, j in 1..3)
--                (a[i] != a[j])

test5 = printExpr $ ArrayComp (Call (UserD "f") [Var "i", Var "j"]) ([(["i"], ArrayComp (Var "k") ([(["k"], mz_interval (IConst 0) (IConst 100))], Just (Bi Eqq (Bi Mod (Var "k") (IConst 7)) (IConst 2)))), (["j"], SetLit [IConst 0, IConst 1])], Nothing)
-- results to: [f(i, j) | i in [k | k in 0..100 where k mod 7 == 2], j in {0, 1}]

test6 = printExpr $ ArrayLit2D [[IConst 1, IConst 2, IConst 3],[IConst 4, IConst 5, IConst 6],[IConst 7, IConst 8, IConst 9]]
-- results to: [| 1, 2, 3
--              | 4, 5, 6
--              | 7, 8, 9|]

test7 = printExpr $ ITE [(Bi mz_lte (Var "x") (Var "y"), Var "x")] (Var "y")
-- results to: if x <= y then x
--             else y endif

test8 = printExpr $ ITE [(Bi Lt (Var "x") (IConst 0), U UMinus (IConst 1)), (Bi Gt (Var "x") (IConst 0), IConst 1)] (IConst 0)
-- results to: if x < 0 then - 1
--             elseif x > 0 then 1
--             else 0 endif

test9 = printExpr $ Let [Declare Dec mz_int "x" (Just (IConst 3)), Declare Dec mz_int "y" (Just (IConst 4))] (Bi mz_plus (Var "x") (Var "y"))
-- results to: let {var int: x = 3;
--                  var int: y = 4;}
--             in x + y

test10 = printItem $ Pred "even" [(Dec, mz_int, "x")] (Just (Bi Eq (Bi Mod (Var "x") (IConst 2)) (IConst 0)))
-- results to: predicate even(var int: x) =
--               x mod 2 = 0;
-}