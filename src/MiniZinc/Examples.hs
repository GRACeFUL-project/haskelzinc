module MiniZinc.Examples where

import MiniZinc.Model
import MiniZinc.Run

default (Int, Float)

aus = satisfy 
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
  where
    nc  = 3
    cv x = x ::: 1 ... nc
    wa  = cv "wa"
    nt  = cv "nt"
    sa  = cv "sa"
    q   = cv "q"
    nsw = cv "nsw"
    v   = cv "v"
    t   = cv "t"

cakes = maximize (400 * b + 450 * c) 
    [ 250 * b + 200 * c .<=. 4000  -- flour
    , 2 * b .<=. 6                 -- bananas
    , 75 * b + 150 * c .<=. 2000   -- sugar
    , 100 * b + 150 * c .<=. 500   -- butter
    , 75 * c .<=. 500              -- cocoa
    ]
  where
    b = "b" ::: 0 ... 100  -- amount of banana cakes
    c = "c" ::: 0 ... 100  -- amount of chocolate cakes

loan i p r = satisfy
    [ b1 .==.  p * (1.0 + i) - r
    , b2 .==. b1 * (1.0 + i) - r
    , b3 .==. b2 * (1.0 + i) - r
    , b4 .==. b3 * (1.0 + i) - r
    ]
  where
    -- intermediate variables
    b1 = "B1" ::: float
    b2 = "B2" ::: float
    b3 = "B3" ::: float
    b4 = "B4" ::: float
