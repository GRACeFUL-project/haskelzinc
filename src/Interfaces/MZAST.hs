{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Interfaces.MZAST (
  include, constraint, output, (%), newline,
  (=.), declare, variable, predicate, function, test, annotation,
  solve, satisfy, minimize, maximize, 
  -- * Types
  (...), ($$),
  -- * Expressions
  -- ** Constants
  true, false, var, int, float, string,
  -- ** Conditional
  if_, then_, elseif_, else_,
  -- ** Sets
  intSet, floatSet, stringSet, mapSet, set, (#/.),
  -- ** Arrays
  boolArray, intArray, floatArray, stringArray,
  boolArray2, intArray2, floatArray2, stringArray2,
  mapArray, mapArray2, array, array2, (#|.), (!.),
  -- ** Comprehension tail
  (@@), where_,
  -- ** Generator calls
  forall,
  -- * Annotations
  (|:),
  module Interfaces.MZASTBase
) where

import Interfaces.MZASTBase

-- Items

newline :: Item
newline = Empty

(%) :: String -> Item
(%) = Comment

include :: String -> Item
include = Include

output :: Expr -> Item
output = Output

constraint :: Expr -> Item
constraint e = Constraint $ AnnExpr e []

solve :: Solve -> Item
solve = Solve

-- Declaring and assigning

infix 1 =.
class Assignable a where
  type Assigned a
  type Body a
  
  (=.) :: a -> Expr -> Assigned a

instance Assignable [Char] where
  type Assigned [Char] = Item
  type Body [Char] = Expr
  name =. e = Assign name $ AnnExpr e []

instance Assignable Declaration where
  type Assigned Declaration = Declaration
  type Body Declaration = Expr
  (Declaration ds ans _) =. e = Declaration ds ans (Just (AnnExpr e []))

declare :: Assigned Declaration -> Item
declare = Declare

declareOnly :: DeclarationSignature -> Declaration
declareOnly ds = Declaration ds [] Nothing

variable :: Inst -> Type -> Ident -> Declaration
variable i t s = declareOnly $ Variable (i, t, s)
{-
enum :: Ident -> Declaration
enum name = declareOnly $ Enum name
-}
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

minimize :: Expr -> Solve
minimize = Minimize []

maximize :: Expr -> Solve
maximize = Maximize []

-- Expressions (plain)

__ :: Expr
__ = AnonVar

var :: Ident -> Expr
var = Var

true :: Expr
true = BConst True

false :: Expr
false = BConst False

int :: Int -> Expr
int = IConst

float :: Float -> Expr
float = FConst

string :: String -> Expr
string = SConst

-- boolSet :: [Bool] -> Expr
-- boolSet = SetLit . (map BConst)

intSet :: [Int] -> Expr
intSet = SetLit . (map IConst)

floatSet :: [Float] -> Expr
floatSet = SetLit . (map FConst)

stringSet :: [String] -> Expr
stringSet = SetLit . (map SConst)

mapSet :: (a -> Expr) -> [a] -> Expr
mapSet f = SetLit . map f

set :: [Expr] -> Expr
set = SetLit

boolArray :: [Bool] -> Expr
boolArray = mapArray BConst

intArray :: [Int] -> Expr
intArray = mapArray IConst

floatArray :: [Float] -> Expr
floatArray = mapArray FConst

stringArray :: [String] -> Expr
stringArray = mapArray SConst

boolArray2 :: [[Bool]] -> Expr
boolArray2 = mapArray2 BConst

intArray2 :: [[Int]] -> Expr
intArray2 = mapArray2 IConst

floatArray2 :: [[Float]] -> Expr
floatArray2 = mapArray2 FConst

stringArray2 :: [[String]] -> Expr
stringArray2 = mapArray2 SConst

-- Creating one- or two-dimensional arrays by mapping
mapArray :: (a -> Expr) -> [a] -> Expr
mapArray f = ArrayLit . map f

mapArray2 :: (a -> Expr) -> [[a]] -> Expr
mapArray2 f = ArrayLit2D . (map (map f))

array :: [Expr] -> Expr
array = ArrayLit

array2 :: [[Expr]] -> Expr
array2 = ArrayLit2D

-- Array comprehension

infix 2 #/., #|.

(#/.) :: Expr -> [CompTail] -> Expr
e #/. cts = SetComp e (mergeCompTails cts)

(#|.) :: Expr -> [CompTail] -> Expr
e #|. cts = ArrayComp e (mergeCompTails cts)

infix 9 !.
(!.) :: Ident -> [Expr] -> Expr
(!.) = ArrayElem

-- Comprehension
-- comprehension tail "i in expr"
infix 4 @@
(@@) :: [Ident] -> Expr -> CompTail
(@@) vars e = ([(vars, e)], Nothing)

--infixl 6 `and_`
combineCompTail :: CompTail -> CompTail -> CompTail
combineCompTail (ins1, me1) (ins2, me2) = (ins1 ++ ins2, decideWhere me1 me2)

mergeCompTails :: [CompTail] -> CompTail
mergeCompTails = foldr1 combineCompTail

decideWhere :: Maybe Expr -> Maybe Expr -> Maybe Expr
decideWhere Nothing   Nothing   = Nothing
decideWhere Nothing   (Just e)  = Just e
decideWhere (Just e)  Nothing   = Just e
decideWhere (Just e1) (Just e2) = Just $ Bi (Op "/\\") e1 e2

infix 6 `where_`
where_ :: CompTail -> Expr -> CompTail
where_ (gs, _) e = (gs, Just e)

-- Generator calls
-- CompTail list makes no sense to be empty
forall :: [CompTail] -> Ident -> Expr -> Expr
forall cts name e = GenCall name (mergeCompTails cts) e

-- Types
infix 3 ...
(...) :: Expr -> Expr -> Type
(...) = Range

($$) :: Ident -> Type
($$) = VarType

-- Auxiliary types for if-then-else expressions

if_ :: Expr -> (Expr -> [(Expr, Expr)])
if_ e = \e1 -> [(e, e1)]

then_ :: (Expr -> [(Expr, Expr)]) -> Expr -> [(Expr, Expr)]
then_ f e = f e

elseif_ :: [(Expr, Expr)] -> Expr -> (Expr -> [(Expr, Expr)])
elseif_ es e = \e1 -> es ++ [(e, e1)]

else_ :: [(Expr, Expr)] -> Expr -> Expr
else_ = ITE

-- Let expressions
let_ = Let

-- Annotations

class Annotatable a where
  (|:) :: a -> [Annotation] -> a

instance Annotatable AnnExpr where
  (AnnExpr e a1) |: a2 = AnnExpr e (a1 ++ a2)

instance Annotatable Declaration where
  (Declaration ds a1 me) |: a2 = Declaration ds (a1 ++ a2) me

instance Annotatable Solve where
  (Satisfy a1)    |: a2 = Satisfy (a1 ++ a2)
  (Minimize a1 e) |: a2 = Minimize (a1 ++ a2) e
  (Maximize a1 e) |: a2 = Maximize (a1 ++ a2) e