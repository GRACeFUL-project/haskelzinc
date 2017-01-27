{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

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
  
  (=.) :: a -> Expr -> Assigned a

instance Assignable [Char] where
  type Assigned [Char] = Item
  name =. e = Assign name $ AnnExpr e []

instance Assignable Declaration where
  type Assigned Declaration = Declaration
  (Declaration ds ans _) =. e = Declaration ds [] (Just (AnnExpr e []))

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

boolSet :: [Bool] -> Expr
boolSet = SetLit . (map BConst)

intSet :: [Int] -> Expr
intSet = SetLit . (map IConst)

floatSet :: [Float] -> Expr
floatSet = SetLit . (map FConst)

stringSet :: [String] -> Expr
stringSet = SetLit . (map SConst)

class SetClass a where
  set :: a -> Expr

instance SetClass [Expr] where
  set = SetLit

instance SetClass (Expr, CompTail) where
  set (e, ct) = SetComp e ct

boolArray :: [Bool] -> Expr
boolArray = arrayMap BConst

intArray :: [Int] -> Expr
intArray = arrayMap IConst

floatArray :: [Float] -> Expr
floatArray = arrayMap FConst

stringArray :: [String] -> Expr
stringArray = arrayMap SConst

boolArray2 :: [[Bool]] -> Expr
boolArray2 = arrayMap2 BConst

intArray2 :: [[Int]] -> Expr
intArray2 = arrayMap2 IConst

floatArray2 :: [[Float]] -> Expr
floatArray2 = arrayMap2 FConst

stringArray2 :: [[String]] -> Expr
stringArray2 = arrayMap2 SConst

-- Creating one- or two-dimensional arrays by mapping
arrayMap :: (a -> Expr) -> [a] -> Expr
arrayMap f = ArrayLit . map f

arrayMap2 :: (a -> Expr) -> [[a]] -> Expr
arrayMap2 f = ArrayLit2D . (map (map f))

array :: [Expr] -> Expr
array = ArrayLit

array2 :: [[Expr]] -> Expr
array2 = ArrayLit2D

-- Array comprehension?

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

iSet :: Ident -> Type
iSet = ACT

-- Auxiliary types for if-then-else expressions

if_ :: Expr -> (Expr -> [(Expr, Expr)])
if_ e = \e1 -> [(e, e1)]

then_ :: (Expr -> [(Expr, Expr)]) -> Expr -> [(Expr, Expr)]
then_ f e = f e

elseif_ :: [(Expr, Expr)] -> Expr -> (Expr -> [(Expr, Expr)])
elseif_ es e = \e1 -> es ++ [(e, e1)]

else_ :: [(Expr, Expr)] -> Expr -> Expr
else_ = ITE

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