{-# LANGUAGE TypeFamilies, FlexibleInstances, AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeFamilies, FlexibleInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Interfaces.MZAST where

import Interfaces.MZASTBase
import Interfaces.MZBuiltIns

-- Items

newline :: Item
newline = Empty

(%) :: String -> Item
(%) = Comment

include :: String -> Item
include = Include

output :: NakedExpr -> Item
output = Output

constraint :: NakedExpr -> Item
constraint e = Constraint $ Expr e []

solve :: Solve -> Item
solve = Solve

-- Declaring and assigning

infixl 1 =.
class Assignable a where
  type Assigned a
  
  (=.) :: a -> NakedExpr -> Assigned a

instance Assignable [Char] where
  type Assigned [Char] = Item
  name =. e = Assign name $ Expr e []

instance Assignable Declaration where
  type Assigned Declaration = Declaration
  (Declaration ds ans _) =. e = Declaration ds [] (Just (Expr e []))

declare :: Assigned Declaration -> Item
declare = Declare

declareOnly :: DeclarationSignature -> Declaration
declareOnly ds = Declaration ds [] Nothing

variable :: Inst -> Type -> Ident -> Declaration
variable i t s = declareOnly $ Variable (i, t, s)

predicate :: Ident -> [Param] -> Declaration
predicate name ps = declareOnly $ Predicate name ps

test :: Ident -> [Param] -> Declaration
test name ps = declareOnly $ Test name ps

function :: Inst -> Type -> Ident -> [Param] -> Declaration
function i t s ps = declareOnly $ Function (i, t, s) ps

annotation :: Ident -> [Param] -> Declaration
annotation i ps = declareOnly $ Annotation' i ps



-- Solve satisfy, minimize, maximize

satisfy :: Solve
satisfy = Satisfy []

minimize :: NakedExpr -> Solve
minimize = Minimize []

maximize :: NakedExpr -> Solve
maximize = Maximize []

-- Expressions (plain)

__ :: NakedExpr
__ = AnonVar

var :: Ident -> NakedExpr
var = Var

true :: NakedExpr
true = BConst True

false :: NakedExpr
false = BConst False

int :: Int -> NakedExpr
int = IConst

float :: Float -> NakedExpr
float = FConst

string :: String -> NakedExpr
string = SConst

boolSet :: [Bool] -> NakedExpr
boolSet = SetLit . (map BConst)

intSet :: [Int] -> NakedExpr
intSet = SetLit . (map IConst)

floatSet :: [Float] -> NakedExpr
floatSet = SetLit . (map FConst)

stringSet :: [String] -> NakedExpr
stringSet = SetLit . (map SConst)

class SetClass a where
  set :: a -> NakedExpr

instance SetClass [NakedExpr] where
  set = SetLit

instance SetClass (NakedExpr, CompTail) where
  set (e, ct) = SetComp e ct

boolArray :: [Bool] -> NakedExpr
boolArray = array BConst

intArray :: [Int] -> NakedExpr
intArray = array IConst

floatArray :: [Float] -> NakedExpr
floatArray = array FConst

stringArray :: [String] -> NakedExpr
stringArray = array SConst

boolArray2 :: [[Bool]] -> NakedExpr
boolArray2 = array2 BConst

intArray2 :: [[Int]] -> NakedExpr
intArray2 = array2 IConst

floatArray2 :: [[Float]] -> NakedExpr
floatArray2 = array2 FConst

stringArray2 :: [[String]] -> NakedExpr
stringArray2 = array2 SConst

-- Creating one- or two-dimensional arrays by mapping
array :: (a -> NakedExpr) -> [a] -> NakedExpr
array f = ArrayLit . map f

array2 :: (a -> NakedExpr) -> [[a]] -> NakedExpr
array2 f = ArrayLit2D . (map (map f))

-- Array comprehension?

infixl 9 #/., #|.

(#/.) :: NakedExpr -> CompTail -> NakedExpr
e #/. ct = SetComp e ct

(#|.) :: NakedExpr -> CompTail -> NakedExpr
e #|. ct = ArrayComp e ct

infixl 2 !.
(!.) :: Ident -> [NakedExpr] -> NakedExpr
(!.) = ArrayElem

-- Comprehension
-- comprehension tail "i in expr"
--infixr 2 @@
(@@) :: [Ident] -> NakedExpr -> CompTail
(@@) vars e = ([(vars, e)], Nothing)

and_ :: CompTail -> CompTail -> CompTail
and_ (ins1, me1) (ins2, me2) = (ins1 ++ ins2, decideWhere me1 me2)

decideWhere :: Maybe NakedExpr -> Maybe NakedExpr -> Maybe NakedExpr
decideWhere Nothing   Nothing   = Nothing
decideWhere Nothing   (Just e)  = Just e
decideWhere (Just e)  Nothing   = Just e
decideWhere (Just e1) (Just e2) = Just $ Bi (Op "/\\") e1 e2

where_ :: CompTail -> NakedExpr -> CompTail
where_ (gs, _) e = (gs, Just e)

-- Generator calls
forall :: CompTail -> Ident -> NakedExpr -> NakedExpr
forall ct name e = GenCall name ct e

-- Types
infix 2 ...
(...) :: NakedExpr -> NakedExpr -> Type
(...) = Range

($$) :: Ident -> Type
($$) = VarType

iSet :: Ident -> Type
iSet = ACT

-- Auxiliary types for if-then-else expressions

data IF_THEN_ELSE = ELSE NakedExpr NakedExpr NakedExpr
data ELSEIF = ELSEIF [(NakedExpr, NakedExpr)] NakedExpr -- ?

if_ :: NakedExpr -> (NakedExpr -> NakedExpr -> IF_THEN_ELSE)
if_ e = \e1 e2 -> ELSE e e1 e2

then_ :: (NakedExpr -> NakedExpr -> IF_THEN_ELSE) -> NakedExpr -> (NakedExpr -> IF_THEN_ELSE)
then_ f e = f e

else_ :: (NakedExpr -> IF_THEN_ELSE) -> NakedExpr -> IF_THEN_ELSE
else_ f e = f e

-- Annotations

class Annotatable a where
  (|:) :: a -> [Annotation] -> a

instance Annotatable Expr where
  (Expr e a1) |: a2 = Expr e (a1 ++ a2)

instance Annotatable Declaration where
  (Declaration ds a1 me) |: a2 = Declaration ds (a1 ++ a2) me

instance Annotatable Solve where
  (Satisfy a1)    |: a2 = Satisfy (a1 ++ a2)
  (Minimize a1 e) |: a2 = Minimize (a1 ++ a2) e
  (Maximize a1 e) |: a2 = Maximize (a1 ++ a2) e