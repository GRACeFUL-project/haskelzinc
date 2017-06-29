{-# LANGUAGE OverloadedStrings #-}

module MExamples where

import Interfaces.MonVar
import Interfaces.MZBuiltIns
import Interfaces.MZASTBase
import Interfaces.MonPrint
import Interfaces.MZinHaskell
import Data.String
import Control.Monad.Except

mUnsatisfiable = do
  var (CT $ int 1 ... int 3) "k"
  constraint $ "k" >. 3
  solve satisfy

mCar = do
  (%) "Example taken from http://hakank.org/minizinc/car.mzn"
  nbCars    <- par Int "nbCars"
  nbOptions <- par Int "nbOptions"
  nbSlots   <- par Int "nbSlots"
  cars      <- par (Set Int) "Cars"
  options   <- par (Set Int) "Options"
  slots     <- par (Set Int) "Slots"
  demand    <- par (Array[ct cars] Dec Int) "demand"
  option    <- par (Array[ct options, ct cars] Dec Int) "option"
  capacity  <- par (Array[ct options, ct $ 1 ... 2] Dec Int) "capacity"
  optionDemand <- par (Array[ct options] Dec Int) "optionDemand"
  
  (%) "decision variables"
  slot  <- var (Array[ct slots] Dec (ct cars)) "slot"
  setup <- var (Array[ct options, ct slots] Dec (ct $ 0 ... 1)) "setup"
  z     <- var Int "z" =. forall [["s"] @@ cars] "sum" ("s" *. slot!.["s"])
  
  solve $ minimize z
  
  constraint $
    forall [["c"] @@ cars] "forall" (
      forall [["s"] @@ slots] "sum" (mz_bool2int[slot!.["s"] =.= "c"])
      =.= demand!.["c"]
    ) /\.
    forall [ ["o"] @@ options
           , ["s"] @@ 1 ... nbSlots -. capacity!.["o", 2] +. 1]
           "forall" (
             forall [["j"] @@ "s" ... ("s" +. capacity!.["o", 2] -. 1)] "sum" (setup!.["o", "j"]) <=. capacity!.["o", 1]
           ) /\.
    forall [["o"] @@ options, ["s"] @@ slots] "forall" (
      setup!.["o", "s"] =.= option!.["o", slot!.["s"]]
    ) /\.
    forall [["o"] @@ options, ["i"] @@ 1 ... optionDemand!.["o"]]
    "forall" (
      forall [["s"] @@ 1 ... nbSlots -. "i" *. capacity!.["o", 2]]
      "sum" (
        setup!.["o", "s"]
      ) >=. (optionDemand!.["o"] -. "i" *. capacity!.["o", 1])
    )
  
  (%) "data"
  nbCars =. 6
  nbOptions =. 5
  nbSlots =. 10
  cars =. 1 ... nbCars
  options =. 1 ... nbOptions
  slots =. 1 ... nbSlots
  demand =. intArray [1, 1, 2, 2, 2, 2]
  option =. mz_array2d[options, cars, intArray [ 1, 0, 0, 0, 1, 1
                                               , 0, 0, 1, 1, 0, 1
                                               , 1, 0, 0, 0, 1, 0
                                               , 1, 1, 0, 1, 0, 0
                                               , 0, 0, 1, 0, 0, 0
                                               ]
                      ]
  capacity =. mz_array2d[options, 1 ... 2, intArray [1, 2, 2, 3, 1, 3, 2, 5, 1, 5]]
  optionDemand =. forall [["j"] @@ cars] "sum" (demand!.["j"] *. option!.["i", "j"]) #|.
                   [["i"] @@ options]
  
mPlanning = do
  nproducts <- par Int "nproducts"
  Products <- par (Set Int) "Products" =. 1 ... nproducts
  
{-
car = [
  (%) "Example taken from http://hakank.org/minizinc/car.mzn",
  par Int "nbCars" =. 6,
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
-}