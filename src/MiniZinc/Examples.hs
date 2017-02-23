-----------------------------------------------------------------------------
-- Copyright 2017, GRACeFUL project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alexg@chalmers.se
-- Stability   :  experimental
-- Portability :  portable (depends on ghc)
--
-- Some examples.
--
-----------------------------------------------------------------------------

module MiniZinc.Examples where

import MiniZinc.Constraint
import MiniZinc.Domain
import MiniZinc.Model
import MiniZinc.Run

import Text.Printf

aus = do 
    wa  <- var range
    nt  <- var range
    sa  <- var range
    q   <- var range
    nsw <- var range
    v   <- var range
    t   <- var range

    let model = satisfy
          [ wa ./=. nt
          , wa ./=. sa
          , nt ./=. sa
          , nt ./=. q
          , sa ./=. q
          , sa ./=. nsw
          , sa ./=. v
          , q  ./=. nsw
          , v  ./=. nsw
          ]
    
    res <- solve model

    let p (n, v) = n ++ "=" ++ getVar res v
    putStr $ unlines $ map p
        [ ("wa", wa), ("nt", nt), ("sa", sa), ("q", q), ("nsw", nsw)
        , ("v", v)
        ]
  where
    nc = 3 :: Int
    range = 1 ... nc

cakes = do
    b <- var $ (0 :: Int) ... 100  -- amount of banana cakes
    c <- var $ (0 :: Int) ... 100  -- amount of chocolate cakes

    let model = maximize (400 * b + 450 * c) 
          [ 250 * b + 200 * c .<=. 4000  -- flour
          , 2 * b .<=. 6                 -- bananas
          , 75 * b + 150 * c .<=. 2000   -- sugar
          , 100 * b + 150 * c .<=. 500   -- butter
          , 75 * c .<=. 500              -- cocoa
          ]

    res <- solve model

    putStr $ unlines 
        [ "no. of banana cakes = " ++ getVar res b
        , "no. of chocolate cakes = " ++ getVar res c
        ]

loan :: Float -> Float -> Float -> IO ()
loan i' p' r' = do
    let p = fromRational $ toRational p'
        i = fromRational $ toRational i'
        r = fromRational $ toRational r'

    -- intermediate variables
    b1 <- var float
    b2 <- var float
    b3 <- var float
    b4 <- var float

    let model = satisfy
          [ b1 .==.  p * (1.0 + i) - r
          , b2 .==. b1 * (1.0 + i) - r
          , b3 .==. b2 * (1.0 + i) - r
          , b4 .==. b3 * (1.0 + i) - r
          ]

    res <- solve model
    let owing = readVar res b4 :: Float

    printf "Borrowing %.2f at %.2f pct interest, and repaying %.2f \n\
           \per quarter for 1 year leaves %.2f owing\n" p' (100*i') r' owing

laplace w h = do 
    t <- array ((0, 0), (w, h)) float

    res <- solve $ satisfy
          [ -- Laplace equation: each internal temp. is avg of its neighbours
            forall [ 4.0 * t(i, j) .==. t(i-1, j) + t(i, j-1) + t(i+1, j) + t(i, j+1)
                   | i <- [1 .. w-1]
                   , j <- [1 .. h-1]
                   ]
          -- Edge constraints
          , forall [ t(i, 0) .==. left   | i <- [1 .. w-1] ]
          , forall [ t(i, h) .==. right  | i <- [1 .. w-1] ]
          , forall [ t(0, j) .==. top    | j <- [1 .. w-1] ]
          , forall [ t(w, j) .==. bottom | j <- [1 .. w-1] ]
          -- Corners
          , t(0, 0) .==. 0.0
          , t(0, h) .==. 0.0
          , t(w, 0) .==. 0.0
          , t(w, h) .==. 0.0
          ]

    putStr $ unlines [concat [getVar res (t(i, j)) ++ " " 
                             | j <- [0 .. h]] 
                             | i <- [0 .. w]]
  where
    left   = 0.0
    right  = 0.0
    top    = 100.0
    bottom = 0.0
