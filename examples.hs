  {- 
      === INSTRUCTIONS ===
    Below are provided the haskelzinc representations models of some MiniZinc constraint models
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

{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Interfaces.MZBuiltIns
import Interfaces.MZinHaskell
import Interfaces.MZAST

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
  assignPar Int "nbCars" 6,
  assignPar Int "nbOptions" 5,
  assignPar Int "nbSlots" 10,
  assignPar (Set Int) "Cars" $ 1 ... "nbCars",
  assignPar (Set Int) "Options" $ 1 ... "nbOptions",
  assignPar (Set Int) "Slots" $ 1 ... "nbSlots",
  assignPar (Array[ctvar "Cars"] Dec Int) "demand"
    $ intArray [1, 1, 2, 2, 2, 2],
  assignPar (Array[ctvar "Options", ctvar "Cars"] Dec Int) "option"
    $ mz_array2d[var "Options", var "Cars", intArray [ 1, 0, 0, 0, 1, 1
                                                      , 0, 0, 1, 1, 0, 1
                                                      , 1, 0, 0, 0, 1, 0
                                                      , 1, 1, 0, 1, 0, 0
                                                      , 0, 0, 1, 0, 0, 0
                                                      ]
         ],
  assignPar (Array[ctvar "Options", CT $ 1 ... 2] Dec Int) "capacity"
    $ mz_array2d[var "Options", 1 ... 2, intArray [1, 2, 2, 3, 1, 3, 2, 5, 1, 5]],
  assignPar (Array[ctvar "Options"] Dec Int) "optionDemand"
    $ forall [["j"] @@ var "Cars"] "sum" ("demand"!.["j"] *. "option"!.["i", "j"]) #|. [["i"] @@ var "Options"],
  (%) "decision variables",
  declareVar (Array[ctvar "Slots"] Dec (ctvar "Cars")) "slot",
  declareVar (Array[ctvar "Options", ctvar "Slots"] Dec (CT $ 0 ... 1)) "setup",
  assignVar Int "z"
    $ forall [["s"] @@ "Cars"] "sum" ("s" *. "slot"!.["s"]),
  solve $ minimize ("z"),
  constraint $
    forall [["c"] @@ "Cars"] "forall" (
      forall [["s"] @@ "Slots"] "sum" (mz_bool2int["slot"!.["s"] =.= "c"])
      =.= "demand"!.["c"]
    ) /\.
    forall [["o"] @@ "Options"
           ,["s"] @@ 1 ... "nbSlots" -. "capacity"!.["o", 2] +. 1]
           "forall" (
      forall [["j"] @@ "s" ... ("s" +. "capacity"!.["o", 2] -. 1)] "sum" ("setup"!.["o", "j"]) <=. "capacity"!.["o", 1]
    ) /\.
    forall [["o"] @@ "Options", ["s"] @@ "Slots"] "forall" (
      "setup"!.["o", "s"] =.= "option"!.["o", "slot"!.["s"]]
    ) /\.
    forall [["o"] @@ "Options", ["i"] @@ 1 ... "optionDemand"!.["o"]]
    "forall" (
      forall [["s"] @@ 1 ... "nbSlots" -. "i" *. "capacity"!.["o", 2]]
      "sum" (
        "setup"!.["o", "s"]
      ) >=. ("optionDemand"!.["o"] -. "i" *. "capacity"!.["o", 1])
    )
  ]
  
evens = [
  (%) "Example taken from http://hakank.org/minizinc/evens.mzn",
  assignPar Int "square" 4,
  assignPar Int "coin" 10,
  assignPar (Set $ CT (1 ... "square")) "S" $ 1 ... "square",
  declareVar (Array [ctvar "S", ctvar "S"] Dec (CT $ 0 ... 1)) "x",
  declareVar (Array [ctvar "S"] Dec Int) "n",
  declareVar (Array [ctvar "S"] Dec Int) "m",
  solve $ satisfy,
  constraint $
    (forall [["i"] @@ "S", ["j"] @@ "S"] "sum" ("x"!.["i", "j"]) =.= "coin")
    /\. forall [["i"] @@ "S"] "forall" (
      forall [["j"] @@ "S"] "sum" ("x"!.["i", "j"]) =.= 2 *. "n"!.["i"]
    )
    /\. forall [["j"] @@ "S"] "forall" (
      forall [["i"] @@ "S"] "sum" ("x"!.["i", "j"]) =.= 2 *. "m"!.["j"])
  ]

divisor225 = [
  (%) "Example taken from http://hakank.org/minizinc/225_divisor.mzn",
  include "globals.mzn",
  assignPar Int "n" 11,
  (%) "decision variables",
  declareVar (Array [CT $ 1 ... "n"] Dec (CT $ 0 ... 1)) "x",
  declareVar (CT $ int 1 ... mz_pow[10, "n"] -. 1) "y",
  declare $ predicate "to_num"[(Dec, Array [Int] Dec Int, "a"), (Dec, Int, "n")]
    =. Let [
      declare $ variable Dec Int "len" =. mz_length["a"]
    ]
    ("n" =.= forall [["i"] @@ 1 ... "len"] "sum" (mz_pow[10, "len" -. "i"] *. "a"!.["i"])),
    solve $ minimize "y"
            |: mz_int_search[E $ "x", A $ mz_first_fail[], A $ mz_indomain_min[], A $ mz_complete[]],
    constraint $ call "to_num" ["x", "y"] /\. "y" `_mod_` 225 =.= 0
    ]

planning = [
  declarePar Int "nproducts",
  assignPar (Set Int) "Products" $ 1 ... "nproducts",
  declarePar (Array [ctvar "Products"] Par Int) "profit",
  declarePar (Array [ctvar "Products"] Par String) "pname",
  declarePar Int "nresources",
  assignPar (Set Int) "Resources" $ 1 ... "nresources",
  declarePar (Array [ctvar "Resources"] Par Int) "capacity",
  declarePar (Array [ctvar "Resources"] Par String) "rname",
  declarePar (Array [ctvar "Products", ctvar "Resources"] Par Int) "consumption",
  constraint $
    mz_assert [mz_forall ["consumption"!.["p", "r"] >=. 0
                         #|. [["r"] @@ "Resources", ["p"] @@ "Products"]]
              , string "Error: Negative consumption"],
  assignPar Int "mproducts" $
    mz_max [mz_min [("capacity"!.["r"] `_div_` "consumption"!.["p", "r"])
                      #|. [(["r"] @@ "Resources")
                          `where_` ("consumption"!.["p", "r"] >=. 0)]]
                   #|. [["p"] @@ "Products"]],
  declareVar (Array [ctvar "Products"] Dec (CT $ 0 ... "mproducts")) "produce",
  declareVar (Array [ctvar "Resources"] Dec (CT $ 0 ... mz_max["capacity"])) "used",
  constraint $ mz_forall [
    ("used"!.["r"] =.= mz_sum ["consumption"!.["p", "r"] *. ("produce"!.["p"]) #|. [["p"] @@ "Products"]])
    /\. ("used"!.["r"] <=. "capacity"!.["r"]) #|. [["r"] @@ "Resources"]],
  solve $ maximize (mz_sum [("profit"!.["p"] *. "produce"!.["p"]) #|. [["p"] @@ "Products"]])
  ]

planningData :: [Item]
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
  declarePar Int "n",
  assignPar (Set Int) "Items" $ 1 ... "n",
  declarePar Int "capacity",
  declarePar (Array [ctvar "Items"] Par Int) "profits",
  declarePar (Array [ctvar "Items"] Par Int) "weights",
  declareVar (Set (ctvar "Items")) "knapsack",
  constraint $
    forall [["i"] @@ "Items"] "sum"
      (mz_bool2int["i" `_in_` "knapsack"] *. "weights"!.["i"])
    <=. "capacity",
  solve $ maximize
    (forall [["i"] @@ "Items"] "sum"
      (mz_bool2int ["i" `_in_` "knapsack"] *. "profits"!.["i"]))
  ]

knapdata :: [Item]
knapdata = [
  "n" =. int 6,
  "capacity" =. int 13,
  "profits" =. intArray [5, 9, 15, 10, 3, 6],
  "weights" =. int 1 ... int 6]

australia = [
  (%) "Colouring Australia using nc colours",
  -- Variable (Par, Int, "nc") =. int 3,
  assignPar Int "nc" 3,
  declareVar (CT $ 1 ... "nc") "wa",
  declareVar (CT $ 1 ... "nc") "nsw",
  declareVar (CT $ 1 ... "nc") "nt",
  declareVar (CT $ 1 ... "nc") "v",
  declareVar (CT $ 1 ... "nc") "sa",
  declareVar (CT $ 1 ... "nc") "t",
  declareVar (CT $ 1 ... "nc") "q",
  constraint $ "wa" !=. "nt",
  constraint $ "wa" !=. "sa",
  constraint $ "nt" !=. "sa",
  constraint $ "nt" !=. "q",
  constraint $ "sa" !=. "q",
  constraint $ "sa" !=. "nsw",
  constraint $ "sa" !=. "v",
  constraint $ "q"  !=. "nsw",
  constraint $ "nsw" !=. "v",
  solve satisfy,
  Output (ArrayLit [
    string "wa=", mz_show ["wa"],
    string "\t nt=", mz_show ["nt"],
    string "\t sa=", mz_show ["sa"],
    string "\n",
    string "q=", mz_show ["q"],
    string "\t nsw=", mz_show ["nsw"],
    string "\t v=", mz_show ["v"],
    string "\n",
    string "t=", mz_show ["t"],
    string "\n"])]

cakes = [
  (%) "Baking cakes for the school fete (with data file)",
  declarePar Int "flour",
  declarePar Int "banana",
  declarePar Int "sugar",
  declarePar Int "butter",
  declarePar Int "cocoa",
  constraint $
    mz_assert ["flour" >=. 0
             ,string "mz_invalid datafile: Ammount of flour is non-negative"],
  constraint $
    mz_assert ["banana" >=. 0
             ,string "mz_invalid datafile: Ammount of banana is non-negative"],
  constraint $
    mz_assert ["sugar" >=. 0
              ,string "mz_invalid datafile: Ammount of sugar is non-negative"],
  constraint $
    mz_assert ["butter" >=. 0
              ,string "mz_invalid datafile: Ammount of butter is non-negative"],
  constraint $
    mz_assert ["cocoa" >=. 0
              ,string "mz_invalid datafile: Ammount of cocoa is non-negative"],
  declareVar (CT $ 0 ... 100) "b",
  declareVar (CT $ 0 ... 100) "c",
  constraint $ 250 *. "b" +. 200 *. "c" <=. "flour",
  constraint $ 2 *. "b" <=. "banana",
  constraint $ 75 *. "b" +. 150 *. "c" <=. "sugar",
  constraint $ 100 *. "b" +. 150 *. "c" <=. "butter",
  constraint $ 75 *. "c" <=. "cocoa",
  (%) "Maximize our profit",
  solve $ maximize (400 *. "b" +. 450 *. "c"),
  output [ string "no. of banana cakes = "
         , mz_show ["b"]
         , string "\n"
         , string "no. of chocolate cakes = "
         , mz_show ["c"], string "\n"
         ]
  ]

cakedata :: [ModelData]
cakedata =
  ["flour"   =. int 4000
  ,"banana"  =. int 6
  ,"sugar"   =. int 2000
  ,"butter"  =. int 500
  ,"cocoa"   =. int 500
  ]

euler1 = [
  (%) "Example taken from http://www.hakank.org/minizinc/euler_1.mzn",
  assignPar Int "n" $ 999,
  declarePar (Array [CT $ 1 ... "n"] Dec (CT $ 0 ... 1)) "x",
  assignVar Int "s"
    $ forall [["i"] @@ 1 ... "n"] "sum" ("x"!.["i"] * "i"),

  solve satisfy,

  constraint $
    forall [["i"] @@ 1 ... "n"] "forall" (
      if_ ("i" `_mod_` 3 =.= 0 \/. "i" `_mod_` 5 =.= 0)
      `then_` ("x"!.["i"] =.= 1)
      `else_` ("x"!.["i"] =.= 0)
  ),

  output [ mz_show ["s"] ]
  ]

euler2 = [
  (%) "Example taken from http://www.hakank.org/minizinc/euler_2.mzn",
  assignPar Int "n" 46,

  declarePar (Array [CT $ 1 ... "n"] Dec Int) "f",
  declarePar (Array [CT $ 1 ... "n"] Dec (CT $ 0 ... 1)) "x",

  assignVar (CT $ 0 ... 10000000) "res"
    $ forall [["i"] @@ 1 ... "n"] "sum" ("x"!.["i"] *. "f"!.["i"]),

  solve satisfy,

  constraint $
    "f"!.[1] =.= 1 /\.
    "f"!.[2] =.= 1 /\.
    forall [["i"] @@ 3 ... "n"] "forall" (
      "f"!.["i"] =.= "f"!.["i" -. 1] +. "f"!.["i" -. 2]
    ),

  constraint $
    forall [["i"] @@ 1 ... "n"] "forall" (
      "f"!.["i"] >. 0 /\.
      ("f"!.["i"] `_mod_` 2 =.= 1 /\. "f"!.["i"] <. 4000000) <->. ("x"!.["i"] =.= 1)
    ),

  output [ mz_show ["f"]
         ++. string "\n"
         ++. mz_show ["x"]
         ++. string "\n"
         ++. string "res: "
         ++. mz_show ["res"]
         ]
  ]

nineDigitArrangement = [
  (%) "Example taken from http://www.hakank.org/minizinc/nine_digit_arrangement.mzn",
  include "globals.mzn",
  declare $ variable Par (Set $ CT $ int 1 ... int 9) "d" =. int 1 ... int 9,
  declare $ variable Dec (ctvar "d") "A",
  declare $ variable Dec (ctvar "d") "B",
  declare $ variable Dec (ctvar "d") "C",
  declare $ variable Dec (ctvar "d") "D",
  declare $ variable Dec (ctvar "d") "E",
  declare $ variable Dec (ctvar "d") "F",
  declare $ variable Dec (ctvar "d") "G",
  declare $ variable Dec (ctvar "d") "H",
  declare $ variable Dec (ctvar "d") "I",

  declare $ variable Dec Int "s",

  declare $ variable Par (Array[ctvar "d"] Dec (ctvar "d")) "x",

  solve $ satisfy |: mz_int_search[E $ var "x", A $ mz_first_fail[], A $ mz_indomain_min[],A $ mz_complete[]],
  constraint $
    call "all_different" [var "x"]
    /\.
    var "s" =.= (int 100 *. var "A" +. int 10 *. var "B" +. var "C") *.
                  (int 10 *. var "D" +. var "E")
    /\.
    var "s" =.= (int 10 *. var "F" +. var "G") *. (int 10 *. var "H" +. var "I")
    /\.
    (int 10 *. var "F" +. var "G") <=. (int 10 *. var "H" +. var "I")
    /\.
    var "s" =.= int 7448,

  output [ string "Solution\n"
         ++. mz_show [var "A"]
         ++. mz_show [var "B"]
         ++. mz_show [var "C"]
         ++. string "\n*  "
         ++. mz_show [var "D"]
         ++. mz_show [var "E"]
         ++. string "\n-----\n "
         ++. mz_show [var "s"]
         ++. string "\n\n   "
         ++. mz_show [var "F"]
         ++. mz_show [var "G"]
         ++. string "\n * "
         ++. mz_show [var "H"]
         ++. mz_show [var "I"]
         ++. string "\n-----\n "
         ++. mz_show [var "s"]
         ++. string "\n"
         ]
  ]
