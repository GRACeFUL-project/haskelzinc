module MiniZinc.Examples where

import MiniZinc.Model
import MiniZinc.Run

aus = do 
    res <- solve model
    let p (n, v) = n ++ "=" ++ getVar res v
    putStr $ unlines $ map p
        [ ("wa", wa), ("nt", nt), ("sa", sa), ("q", q), ("nsw", nsw)
        , ("v", v)
        ]
  where
    nc  = 3 :: Int
    range = 1 ... nc
    wa  = var range
    nt  = var range
    sa  = var range
    q   = var range
    nsw = var range
    v   = var range
    t   = var range

    model = satisfy
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

cakes = do
    res <- solve model
    putStr $ unlines 
        [ "no. of banana cakes = " ++ getVar res b
        , "no. of chocolate cakes = " ++ getVar res c
        ]
  where
    b = var $ (0 :: Int) ... 100  -- amount of banana cakes
    c = var $ (0 :: Int) ... 100  -- amount of chocolate cakes

    model = maximize (400 * b + 450 * c) 
        [ 250 * b + 200 * c .<=. 4000  -- flour
        , 2 * b .<=. 6                 -- bananas
        , 75 * b + 150 * c .<=. 2000   -- sugar
        , 100 * b + 150 * c .<=. 500   -- butter
        , 75 * c .<=. 500              -- cocoa
        ]

loan i p r = do
    res <- solve model
    putStr $ unlines
        [ "Borrowing " ++ emit p ++ " at " ++ emit (i * 100.0) ++ "% interest, and repaying " ++ emit r
        , "per quarter for 1 year leaves " ++ getVar res b4 ++ " owing"
        ]
  where
    -- intermediate variables
    b1 = var float
    b2 = var float
    b3 = var float
    b4 = var float

    model = satisfy
        [ b1 .==.  p * (1.0 + i) - r
        , b2 .==. b1 * (1.0 + i) - r
        , b3 .==. b2 * (1.0 + i) - r
        , b4 .==. b3 * (1.0 + i) - r
        ]

laplace w h = satisfy
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
  where
    t      = arr2 w h float
    left   = 0.0
    right  = 0.0
    top    = 100.0
    bottom = 0.0

arr2 n m domain = \(i, j) -> xs !! i !! j
  where
    xs = [[var domain | i <- [0 .. n]] | j <- [0 .. m]] 

{-
prodPlanning nproducts nresources = maximize (undefined)
    [
    ]
  where
    products = undefined
-}
