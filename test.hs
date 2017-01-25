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

default (Int, Float)

small = mz_abs [int 3, int 5]
big   =
  mz_discrete_distribution [mz_dom_bounds_array [var "somebigarrayname"]
                           ,var "areallybigset" `_intersect_` var "coulditneedmore?"]

{-  
unsatisfiable =[
  Declare $ Declaration (Variable (Dec, (Range (IConst 1) (IConst 3)), "k")) [] Nothing,
  --var (Dec, (Range (IConst 1) (IConst 3))) "k",
  Constraint $ Expr (Bi mz_gt (Var "k") (IConst 3)) [],
  Solve [] Satisfy
  ]
-}
planning = [
  variable Par Int "nproducts",
  Variable (Par, Set Int, "Products") =. int 1 -.- var "nproducts",
  variable Par (Array [ACT "Products"] (Par, Int)) "profit",
  variable Par (Array [ACT "Products"] (Par, String)) "pname",
  variable Par Int "nresources",
  Variable (Par, Set Int, "Resources") =. int 1 -.- var "nresources",
  variable Par (Array [ACT "Resources"] (Par, Int)) "capacity",
  variable Par (Array [ACT "Resources"] (Par, String)) "rname",
  variable Dec (Array [ACT "Products", ACT "Resources"] (Par, Int)) "consumption",
  constraint $
    mz_assert [mz_forall [ArrayComp (("consumption"!.[Var "p", Var "r"]) >=. int 0) ((["r"] @@ var "Resources") `and_` (["p"] @@ var "Products"))], string "Error: Negative consumption"],
  Variable (Par, Int, "mproducts") =.
      mz_max [ArrayComp (mz_min [ArrayComp (("capacity"!.[Var "r"]) `_div_` ("consumption"!.[var "p", var "r"])) (["r"] @@ var "Resources" `where_` (("consumption"!.[var "p", var "r"]) >=. int 0))]) (["p"] @@ var "Products")],
  variable Dec (Array [ACT "Products"] (Dec, int 0 ... var "mproducts")) "produce",
  variable Dec (Array [ACT "Resources"] (Dec, int 0 ... mz_max[var "capacity"])) "used",
  constraint $ mz_forall [
    (("used"!.[var "r"]) =.= (mz_sum [("consumption"!.[var "p", var "r"]) *. ("produce"!.[var "p"]) #|. (["p"] @@ var "Products")]))
    /\. (("used"!.[var "r"]) <=. ("capacity"!.[var "r"])) #|. (["r"] @@ var "Resources")],
  solve $ maximize (mz_sum [(("profit"!.[Var "p"]) *. ("produce"!.[var "p"])) #|. (["p"] @@ var "Products")])
  ]

planningData = [
  "nproducts" =. int 2,
  "profit" =. intArray [400, 450],
  "pname" =. stringArray ["banana-cake", "chocolate-cake"],
  "nresources" =. int 5,
  "capacity" =. intArray [4000, 6, 2000, 500, 500],
  "rname" =. stringArray ["flour", "banana", "sugar", "butter", "cocoa"],
  "consumption" =. intArray2 [[250, 2,  75, 100,  0]
                             ,[200, 0, 150, 150, 75]]
  ]

knapsack = [
  variable Par Int "n",
  Variable (Par, Set Int, "Items") =. (int 1) -.- (var "n"),
  variable Par Int "capacity",
  Empty,
  variable Par (Array [ACT "Items"] (Par, Int)) "profits",
  variable Par (Array [ACT "Items"] (Par, Int)) "weights",
  Empty,
  variable Dec (Set (ACT "Items")) "knapsack",
  Empty,
  constraint $ (forall (["i"] @@ var "Items") "sum" ((mz_bool2int [var "i" `_in_` var "knapsack"]) *. ("weights"!.[var "i"]))) <=. var "capacity",
  Empty,
  solve $ maximize (forall (["i"] @@ var "Items") "sum" (mz_bool2int [var "i" `_in_` var "knapsack"] *. ("profits"!.[Var "i"])))]

knapdata = [
  "n" =. int 6,
  "capacity" =. int 13,
  "profits" =. intArray [5, 9, 15, 10, 3, 6],
  "weights" =. int 1 -.- int 6]

australia = [
  (%%) "Colouring Australia using nc colours",
  Variable (Par, Int, "nc") =. int 3,
  variable Dec (int 1 ... var "nc") "wa",
  variable Dec (int 1 ... var "nc") "nsw",
  variable Dec (int 1 ... var "nc") "nt",
  variable Dec (int 1 ... var "nc") "v",
  variable Dec (int 1 ... var "nc") "sa",
  variable Dec (int 1 ... var "nc") "t",
  variable Dec (int 1 ... var "nc") "q",
  constraint $ var "wa" !=. var "nt",
  constraint $ var "wa" !=. var "sa",
  constraint $ var "nt" !=. var "sa",
  constraint $ var "nt" !=. var "q",
  constraint $ var "sa" !=. var "q",
  constraint $ var "sa" !=. var "nsw",
  constraint $ var "sa" !=. var "v",
  constraint $ var "q"  !=. var "nsw",
  constraint $ var "nsw" !=. var "v",
  solve satisfy,
  Output (ArrayLit [
    string "wa=", mz_show [var "wa"],
    string "\t nt=", mz_show [var "nt"],
    string "\t sa=", mz_show [var "sa"],
    string "\n",
    string "q=", mz_show [var "q"],
    string "\t nsw=", mz_show [var "nsw"],
    string "\t v=", mz_show [var "v"],
    string "\n",
    string "t=", mz_show [var "t"],
    string "\n"])]

cakes = [
  (%%) "Baking cakes for the school fete (with data file)",
  variable Par Int "flour",
  variable Par Int "banana",
  variable Par Int "sugar",
  variable Par Int "butter",
  variable Par Int "cocoa",
  constraint $ 
    mz_assert [var "flour" >=. int 0
             ,string "mz_invalid datafile: Ammount of flour is non-negative"],
  constraint $
    mz_assert [var "banana" >=. int 0
             ,string "mz_invalid datafile: Ammount of banana is non-negative"],
  constraint $
    mz_assert [var "sugar" >=. int 0
              ,string "mz_invalid datafile: Ammount of sugar is non-negative"],
  constraint $
    mz_assert [var "butter" >=. int 0
              ,string "mz_invalid datafile: Ammount of butter is non-negative"],
  constraint $
    mz_assert [var "cocoa" >=. int 0
              ,string "mz_invalid datafile: Ammount of cocoa is non-negative"],
  variable Dec (int 0 ... int 100) "b",
  variable Dec (int 0 ... int 100) "c",
  constraint $ int 250 *. var "b" +. int 200 *. var "c" <=. var "flour",
  constraint $ int 2 *. var "b" <=. var "banana",
  constraint $ int 75 *. var "b" +. int 150 *. var "c" <=. var "sugar",
  constraint $ int 100 *. var "b" +. int 150 *. var "c" <=. var "butter",
  constraint $ int 75 *. var "c" <=. var "cocoa",
  (%%) "Maximize our profit",
  solve $ maximize (int 400 *. var "b" +. int 450 *. var "c"),
  output (ArrayLit
    [string "no. of banana cakes = "
    ,mz_show [var "b"]
    ,string "\n"
    ,string "no. of chocolate cakes = "
    ,mz_show [var "c"], string "\n"
    ])]
    
cakedata =
  ["flour"   =. int 4000
  ,"banana"  =. int 6
  ,"sugar"   =. int 2000
  ,"butter"  =. int 500
  ,"cocoa"   =. int 500
  ]
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