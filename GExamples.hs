{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
-- {-# AllowAmbiguousTypes #-}
-- {-# Safe #-}

module GExamples where

import Interfaces.MZBuiltIns
import Interfaces.MZAST
import Interfaces.MZinHaskell
import Data.String

-- default (Int, Float)

-- main = runModel divisor225 "divisor225.mzn" 1 10

nineDigitArrangement = [
  (%) "Example taken from http://www.hakank.org/minizinc/nine_digit_arrangement.mzn",
  include "globals.mzn",
  par (Set $ CT $ 1 ... 9) "d" =. 1 ... 9,
  var (ctvar "d") "A",
  var (ctvar "d") "B",
  var (ctvar "d") "C",
  var (ctvar "d") "D",
  var (ctvar "d") "E",
  var (ctvar "d") "F",
  var (ctvar "d") "G",
  var (ctvar "d") "H",
  var (ctvar "d") "I",
     
  var Int "s",

  var (Array[ctvar "d"] Dec (ctvar "d")) "x" =. array["A", "B", "C", "D", "E", "F", "G", "H", "I"],

  solve $ satisfy |: mz_int_search[E $ "x", A $ mz_first_fail[], A $ mz_indomain_min[],A $ mz_complete[]],
  constraint $
    mz_all_different["x"]
    /\.
    "s" =.= (100 *. "A" +. 10 *. "B" +. "C") *.
                  (10 *. "D" +. "E")
    /\.
    "s" =.= (10 *. "F" +. "G") *. (10 *. "H" +. "I")
    /\.
    (10 *. "F" +. "G") <=. (10 *. "H" +. "I")
    /\.
    "s" =.= int 7448,

  output [ string "Solution\n"
         ++. mz_show ["A"]
         ++. mz_show ["B"]
         ++. mz_show ["C"]
         ++. string "\n*  "
         ++. mz_show ["D"]
         ++. mz_show ["E"]
         ++. string "\n-----\n "
         ++. mz_show ["s"]
         ++. string "\n\n   "
         ++. mz_show ["F"]
         ++. mz_show ["G"]
         ++. string "\n * "
         ++. mz_show ["H"]
         ++. mz_show ["I"]
         ++. string "\n-----\n "
         ++. mz_show ["s"]
         ++. string "\n"
         ]
  ]

unsatisfiable = [
    var (CT $ 1 ... 3) "k",
    constraint $ "k" >. 3,
    solve satisfy
  ]

car = [
  (%) "Example taken from http://hakank.org/minizinc/car.mzn",
  par Int "nbCars" =. 6,
  par Int "nbOptions" =. 5,
  par Int "nbSlots" =. 10,
  par (Set Int) "Cars" =. 1 ... "nbCars",
  par (Set Int) "Options" =. 1 ... "nbOptions",
  par (Set Int) "Slots" =. 1 ... "nbSlots",
  par (Array[ctvar "Cars"] Dec Int) "demand"
    =. intArray [1, 1, 2, 2, 2, 2],
  par (Array[ctvar "Options", ctvar "Cars"] Dec Int) "option"
    =. mz_array2d["Options", "Cars", intArray [ 1, 0, 0, 0, 1, 1
                                             , 0, 0, 1, 1, 0, 1
                                             , 1, 0, 0, 0, 1, 0
                                             , 1, 1, 0, 1, 0, 0
                                             , 0, 0, 1, 0, 0, 0
                                             ]
         ],
  par (Array[ctvar "Options", CT $ 1 ... 2] Dec Int) "capacity"
    =. mz_array2d["Options", 1 ... 2, intArray [1, 2, 2, 3, 1, 3, 2, 5, 1, 5]],
  par (Array[ctvar "Options"] Dec Int) "optionDemand"
    =. forall [["j"] @@ "Cars"] "sum" ("demand"!.["j"] *. "option"!.["i", "j"]) #|. [["i"] @@ "Options"],
  (%) "decision variables",
  var (Array[ctvar "Slots"] Dec (ctvar "Cars")) "slot",
  var (Array[ctvar "Options", ctvar "Slots"] Dec (CT $ 0 ... 1)) "setup",
  var Int "z"
    =. forall [["s"] @@ "Cars"] "sum" ("s" *. "slot"!.["s"]),
  solve (minimize "z"),
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
  par Int "square" =. 4,
  par Int "coin" =. 10,
  par (Set $ CT (1 ... "square")) "S" =. 1 ... "square",
  var (Array [ctvar "S", ctvar "S"] Dec (CT $ 0 ... 1)) "x",
  var (Array [ctvar "S"] Dec Int) "n",
  var (Array [ctvar "S"] Dec Int) "m",
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
  par Int "n" =. 11,
  (%) "decision variables",
  var (Array [CT $ 1 ... "n"] Dec (CT $ 0 ... 1)) "x",
  var (CT $ int 1 ... mz_pow[10, "n"] -. 1) "y",
  predicate "to_num"[var (Array [Int] Dec Int) "a", var Int "n"]
    =. let_ [
      var Int "len" =. mz_length["a"]
    ]
    ("n" =.= forall [["i"] @@ 1 ... "len"] "sum" (mz_pow[10, "len" -. "i"] *. "a"!.["i"])),
    solve $ minimize "y"
            |: mz_int_search[E $ "x", A $ mz_first_fail[], A $ mz_indomain_min[], A $ mz_complete[]],
    constraint $ prefCall "to_num" ["x", "y"] /\. "y" `_mod_` 225 =.= 0
    ]

planning = [
  par Int "nproducts",
  par (Set Int) "Products" =. 1 ... "nproducts",
  par (Array [ctvar "Products"] Par Int) "profit",
  par (Array [ctvar "Products"] Par String) "pname",
  par Int "nresources",
  par (Set Int) "Resources" =. 1 ... "nresources",
  par (Array [ctvar "Resources"] Par Int) "capacity",
  par (Array [ctvar "Resources"] Par String) "rname",
  par (Array [ctvar "Products", ctvar "Resources"] Par Int) "consumption",
  constraint $
    mz_assert [mz_forall ["consumption"!.["p", "r"] >=. 0
                         #|. [["r"] @@ "Resources", ["p"] @@ "Products"]]
              , string "Error: Negative consumption"],
  par Int "mproducts" =.
    mz_max [mz_min [("capacity"!.["r"] `_div_` "consumption"!.["p", "r"])
                      #|. [(["r"] @@ "Resources")
                          `where_` ("consumption"!.["p", "r"] >=. 0)]]
                   #|. [["p"] @@ "Products"]],
  var (Array [ctvar "Products"] Dec (CT $ 0 ... "mproducts")) "produce",
  var (Array [ctvar "Resources"] Dec (CT $ 0 ... mz_max["capacity"])) "used",
  constraint $ mz_forall [
    ("used"!.["r"] =.= mz_sum ["consumption"!.["p", "r"] *. ("produce"!.["p"]) #|. [["p"] @@ "Products"]])
    /\. ("used"!.["r"] <=. "capacity"!.["r"]) #|. [["r"] @@ "Resources"]],
  solve $ maximize (mz_sum [("profit"!.["p"] *. "produce"!.["p"]) #|. [["p"] @@ "Products"]])
  ]

australia = [
  (%) "Colouring Australia using nc colours",
  par Int "nc" =. 3,
  
  var (CT $ 1 ... "nc") "wa",
  var (CT $ 1 ... "nc") "nsw",
  var (CT $ 1 ... "nc") "nt",
  var (CT $ 1 ... "nc") "v",
  var (CT $ 1 ... "nc") "sa",
  var (CT $ 1 ... "nc") "t",
  var (CT $ 1 ... "nc") "q",
  
  constraint $ "wa" !=. "nt",
  constraint $ "wa" !=. "sa",
  constraint $ "nt" !=. "sa",
  constraint $ "nt" !=. "q",
  constraint $ "sa" !=. "q",
  constraint $ "sa" !=. "nsw",
  constraint $ "sa" !=. "v",
  constraint $ "q"  !=. "nsw",
  constraint $ "nsw" !=. "v",
  solve satisfy
  ]

cakes = [
  (%) "Baking cakes for the school fete (with data file)",
  par Int "flour" =. int 4000,
  par Int "banana" =. int 6,
  par Int "sugar" =. int 2000,
  par Int "butter" =. int 500,
  par Int "cocoa" =. int 500,
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
  var (CT $ 0 ... 100) "b",
  var (CT $ 0 ... 100) "c",
  constraint $ 250 *. "b" +. 200 *. "c" <=. "flour",
  constraint $ 2 *. "b" <=. "banana",
  constraint $ 75 *. "b" +. 150 *. "c" <=. "sugar",
  constraint $ 100 *. "b" +. 150 *. "c" <=. "butter",
  constraint $ 75 *. "c" <=. "cocoa",
  (%) "Maximize our profit",
  solve $ maximize (400 *. "b" +. 450 *. "c")
  ]
