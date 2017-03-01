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
import Interfaces.MZAST
import Interfaces.MZPrinter

default (Int, Float)

klara = declare $ 
  variable Dec Int "varname"
  |: mz_int_search[A $ mz_domain[], E $ intArray [1, 2, 3, 4]]
  |: mz_bool_search[A $ mz_domain[], E $ intArray [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0]]
  =. int 4
  
small = mz_abs [int 3, int 5]
big   =
  mz_discrete_distribution [mz_dom_bounds_array [var "somebigarrayname"]
                           ,var "areallybigset" `_intersect_` var "coulditneedmore?"]

unsatisfiable = [
    declare $ variable Dec (CT $ int 1 ... int 3) "k",
    constraint $ var "k" >. int 3,
    solve $ satisfy
  ]
  
car = [
  (%) "Example taken from http://hakank.org/minizinc/car.mzn",
  declare $ variable Par Int "nbCars" =. int 6,
  declare $ variable Par Int "nbOptions" =. int 5,
  declare $ variable Par Int "nbSlots" =. int 10,
  declare $ variable Par (Set Int) "Cars" =. int 1 ... var "nbCars",
  declare $ variable Par (Set Int) "Options" =. int 1 ... var "nbOptions",
  declare $ variable Par (Set Int) "Slots" =. int 1 ... var "nbSlots",
  declare $ variable Par (Array[ctvar "Cars"] Dec Int) "demand"
    =. intArray [1, 1, 2, 2, 2, 2],
  declare $ variable Par (Array[ctvar "Options", ctvar "Cars"] Dec Int) "option"
    =. mz_array2d[var "Options", var "Cars", intArray [ 1, 0, 0, 0, 1, 1
                                                      , 0, 0, 1, 1, 0, 1
                                                      , 1, 0, 0, 0, 1, 0
                                                      , 1, 1, 0, 1, 0, 0
                                                      , 0, 0, 1, 0, 0, 0
                                                      ]
         ],
  declare $ variable Par (Array[ctvar "Options", CT $ int 1 ... int 2] Dec Int) "capacity"
    =. mz_array2d[var "Options", int 1 ... int 2, intArray [1, 2, 2, 3, 1, 3, 2, 5, 1, 5]],
  declare $ variable Dec (Array[ctvar "Options"] Dec Int) "optionDemand"
    =. forall [["j"] @@ var "Cars"] "sum" ("demand"!.[var "j"] *. "option"!.[var "i", var "j"]) #|. [["i"] @@ var "Options"],
  (%) "decision variables",
  declare $ variable Dec (Array[ctvar "Slots"] Dec (ctvar "Cars")) "slot",
  declare $ variable Dec (Array[ctvar "Options", ctvar "Slots"] Dec (CT $ int 0 ... int 1)) "setup",
  declare $ variable Dec Int "z" 
    =. forall [["s"] @@ var "Cars"] "sum" (var "s" *. "slot"!.[var "s"]),
  solve $ minimize (var "z"),
  constraint $
    forall [["c"] @@ var "Cars"] "forall" (
      forall [["s"] @@ var "Slots"] "sum" (mz_bool2int["slot"!.[var "s"] =.= var "c"])
      =.= "demand"!.[var "c"]
    ) /\.
    forall [["o"] @@ var "Options"
           ,["s"] @@ int 1 ... var "nbSlots" -. "capacity"!.[var "o", int 2] +. int 1]
           "forall" (
      forall [["j"] @@ var "s" ... (var "s" +. "capacity"!.[var "o", int 2] -. int 1)] "sum" ("setup"!.[var "o", var "j"]) <=. "capacity"!.[var "o", int 1]
    ) /\.
    forall [["o"] @@ var "Options", ["s"] @@ var "Slots"] "forall" (
      "setup"!.[var "o", var "s"] =.= "option"!.[var "o", "slot"!.[var "s"]]
    ) /\.
    forall [["o"] @@ var "Options", ["i"] @@ int 1 ... "optionDemand"!.[var "o"]]
    "forall" (
      forall [["s"] @@ int 1 ... var "nbSlots" -. var "i" *. "capacity"!.[var "o", int 2]]
      "sum" (
        "setup"!.[var "o", var "s"]
      ) >=. ("optionDemand"!.[var "o"] -. var "i" *. "capacity"!.[var "o", int 1])
    )
  ]
  
evens = [
  (%) "Example taken from http://hakank.org/minizinc/evens.mzn",
  declare $ variable Par Int "square" =. int 4,
  declare $ variable Par Int "coin" =. int 10,
  declare $ variable Par (Set $ CT (int 1 ... var "square")) "S" =. int 1 ... var "square",
  declare $ variable Dec (Array [ctvar "S", ctvar "S"] Dec (CT $ int 0 ... int 1)) "x",
  declare $ variable Dec (Array [ctvar "S"] Dec Int) "n",
  declare $ variable Dec (Array [ctvar "S"] Dec Int) "m",
  solve $ satisfy,
  constraint $ 
    (forall [["i"] @@ var "S", ["j"] @@ var "S"] "sum" ("x"!.[var"i", var "j"]) =.= var "coin")
    /\. forall [["i"] @@ var "S"] "forall" (
      forall [["j"] @@ var "S"] "sum" ("x"!.[var"i", var "j"]) =.= int 2 *. "n"!.[var "i"]
    )
    /\. forall [["j"] @@ var "S"] "forall" (
      forall [["i"] @@ var "S"] "sum" ("x"!.[var "i", var "j"]) =.= int 2 *. "m"!.[var "j"])
  ]

divisor225 = [
  (%) "Example taken from http://hakank.org/minizinc/225_divisor.mzn",
  include "globals.mzn",
  declare $ variable Par Int "n" =. int 11,
  (%) "decision variables",
  declare $ variable Dec (Array [CT $ int 1 ... var "n"] Dec (CT $ int 0 ... int 1)) "x",
  declare $ variable Dec (CT $ int 1 ... mz_pow[int 10, var "n"] -. int 1) "y",
  declare $ predicate "to_num"[(Dec, Array [Int] Dec Int, "a"), (Dec, Int, "n")]
    =. Let [
      declare $ variable Dec Int "len" =. mz_length[var "a"]
    ]
    (var "n" =.= forall [["i"] @@ int 1 ... var "len"] "sum" (mz_pow[int 10, var "len" -. var "i"] *. "a"!.[var "i"])),
    solve $ minimize (var "y")
            |: mz_int_search[E $ var "x", A $ mz_first_fail[], A $ mz_indomain_min[], A $ mz_complete[]],
    constraint $ call "to_num" [var "x", var "y"] /\. (var "y") `_mod_` (int 225) =.= int 0
    ]

planning = [
  declare $ variable Par Int "nproducts",
  declare $ variable Par (Set Int) "Products" =. int 1 ... var "nproducts",
  declare $ variable Par (Array [ctvar "Products"] Par Int) "profit",
  declare $ variable Par (Array [ctvar "Products"] Par String) "pname",
  declare $ variable Par Int "nresources",
  declare $ variable Par (Set Int) "Resources" =. int 1 ... var "nresources",
  declare $ variable Par (Array [ctvar "Resources"] Par Int) "capacity",
  declare $ variable Par (Array [ctvar "Resources"] Par String) "rname",
  declare $ variable Dec (Array [ctvar "Products", ctvar "Resources"] Par Int) "consumption",
  constraint $
    mz_assert [mz_forall ["consumption"!.[Var "p", Var "r"] >=. int 0 
                         #|. [["r"] @@ var "Resources", ["p"] @@ var "Products"]]
              , string "Error: Negative consumption"],
  declare $ variable Par Int "mproducts" =.
    mz_max [mz_min [("capacity"!.[Var "r"] `_div_` "consumption"!.[var "p", var "r"]) 
                      #|. [(["r"] @@ var "Resources") 
                          `where_` ("consumption"!.[var "p", var "r"] >=. int 0)]] 
                   #|. [["p"] @@ var "Products"]],
  declare $ variable Dec (Array [ctvar "Products"] Dec (CT $ int 0 ... var "mproducts")) "produce",
  declare $ variable Dec (Array [ctvar "Resources"] Dec (CT $ int 0 ... mz_max[var "capacity"])) "used",
  constraint $ mz_forall [
    ("used"!.[var "r"] =.= mz_sum ["consumption"!.[var "p", var "r"] *. ("produce"!.[var "p"]) #|. [["p"] @@ var "Products"]])
    /\. ("used"!.[var "r"] <=. "capacity"!.[var "r"]) #|. [["r"] @@ var "Resources"]],
  solve $ maximize (mz_sum [("profit"!.[Var "p"] *. "produce"!.[var "p"]) #|. [["p"] @@ var "Products"]])
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
  declare $ variable Par Int "n",
  declare $ variable Par (Set Int) "Items" =. int 1 ... var "n",
  declare $ variable Par Int "capacity",
  declare $ variable Par (Array [ctvar "Items"] Par Int) "profits",
  declare $ variable Par (Array [ctvar "Items"] Par Int) "weights",
  declare $ variable Dec (Set (ctvar "Items")) "knapsack",
  constraint $ 
    forall [["i"] @@ var "Items"] "sum" 
      (mz_bool2int[var "i" `_in_` var "knapsack"] *. "weights"!.[var "i"]) 
    <=. var "capacity",
  solve $ maximize 
    (forall [["i"] @@ var "Items"] "sum" 
      (mz_bool2int [var "i" `_in_` var "knapsack"] *. "profits"!.[Var "i"]))
  ]

knapdata = [
  "n" =. int 6,
  "capacity" =. int 13,
  "profits" =. intArray [5, 9, 15, 10, 3, 6],
  "weights" =. int 1 ... int 6]

australia = [
  (%) "Colouring Australia using nc colours",
  -- Variable (Par, Int, "nc") =. int 3,
  declare $ variable Par Int "nc" =. int 3,
  declare $ variable Dec (CT $ int 1 ... var "nc") "wa",
  declare $ variable Dec (CT $ int 1 ... var "nc") "nsw",
  declare $ variable Dec (CT $ int 1 ... var "nc") "nt",
  declare $ variable Dec (CT $ int 1 ... var "nc") "v",
  declare $ variable Dec (CT $ int 1 ... var "nc") "sa",
  declare $ variable Dec (CT $ int 1 ... var "nc") "t",
  declare $ variable Dec (CT $ int 1 ... var "nc") "q",
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
  (%) "Baking cakes for the school fete (with data file)",
  declare $ variable Par Int "flour",
  declare $ variable Par Int "banana",
  declare $ variable Par Int "sugar",
  declare $ variable Par Int "butter",
  declare $ variable Par Int "cocoa",
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
  declare $ variable Dec (CT $ int 0 ... int 100) "b",
  declare $ variable Dec (CT $ int 0 ... int 100) "c",
  constraint $ int 250 *. var "b" +. int 200 *. var "c" <=. var "flour",
  constraint $ int 2 *. var "b" <=. var "banana",
  constraint $ int 75 *. var "b" +. int 150 *. var "c" <=. var "sugar",
  constraint $ int 100 *. var "b" +. int 150 *. var "c" <=. var "butter",
  constraint $ int 75 *. var "c" <=. var "cocoa",
  (%) "Maximize our profit",
  solve $ maximize (int 400 *. var "b" +. int 450 *. var "c"),
  output [ string "no. of banana cakes = "
         , mz_show [var "b"]
         , string "\n"
         , string "no. of chocolate cakes = "
         , mz_show [var "c"], string "\n"
         ]
  ]
    
cakedata =
  ["flour"   =. int 4000
  ,"banana"  =. int 6
  ,"sugar"   =. int 2000
  ,"butter"  =. int 500
  ,"cocoa"   =. int 500
  ]