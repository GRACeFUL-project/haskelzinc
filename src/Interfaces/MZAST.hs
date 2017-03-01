{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

{-|
Module      : MZAST
Description : More human-friendly interface for "Interfaces.MZASTBase"
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental


This module defines a more human-friendly interface for the MiniZinc 2.1 language, on top
of "Interfaces.MZASTBase". With the use of this module, one can represent MiniZinc models in Haskell code.
-}
 
module Interfaces.MZAST (
  -- * Items
  include, constraint, output, (%),
  (=.), declare, variable, predicate, function, test, annotation,
  solve, satisfy, minimize, maximize,
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
  -- * Types
  ctvar, ($$),
  -- * Annotations
  (|:),
  module Interfaces.MZASTBase
) where

import Interfaces.MZASTBase

-- Items
{-
-- | Represents an empty line in the MiniZinc code.
newline :: Item
newline = Empty
-}

-- | Represents a comment in the MiniZinc model.
-- Example:
-- 
-- >>> (%) "comment goes here"
-- % comment goes here
(%) :: String -> Item
(%) = Comment

-- | Represents an include item in the MiniZinc model. The argument is the filepath.
include :: String -> Item
include = Include

-- | Represents an output item in the MiniZinc model. The elements in the list argument 
-- represent the elements of the MiniZinc array passed to the output item.
-- 
-- Example:
--  
-- >>> output [string "x = ", mz_show[var "x"]]
-- output ["x = ", show(x)];
--
-- If the represented model contains an @output@ item that changes the default format of
-- the solver's solutions, then a custom parser will be needed to get the solver's results
-- back in Haskell. See "Interfaces.FZSolutionParser".
output :: [Expr] -> Item
output = Output . ArrayLit

-- | Represents a non-annotated constraint item in the MiniZinc model.
constraint :: Expr -> Item
constraint e = Constraint $ AnnExpr e []

-- | Represents a solve item in the MiniZinc model. Used together with one of 'satisfy',
-- 'minimize' or 'maximize' functions.
solve :: Solve -> Item
solve = Solve

-- | Finilizes the representation of a non-annotated solve item. Use '|:' operator to
-- annotate it.
satisfy :: Solve
satisfy = Satisfy []

-- | Finilizes the representation of a non-annotated solve item. Use '|:' operator to
-- annotate it.
minimize :: Expr -> Solve
minimize = Minimize []

-- | Finilizes the representation of a non-annotated solve item. Use '|:' operator to
-- annotate it.
maximize :: Expr -> Solve
maximize = Maximize []

-- Declaring and assigning

infix 1 =.

class Assignable a where
  type Assigned a
  
  -- | The operator that represents assignment in MiniZinc code. One can assign a non-
  -- annotated expression to a variable, predicate, test or function either on declaration
  -- or later.
  -- 
  -- To annotate the expression, use haskelzinc operator '|:'.
  --
  -- Examples:
  -- 
  -- To assign to an already declared variable, predicate, test or function 
  -- @x@, use:
  -- 
  -- >>> "x" =. int 1
  --
  -- To assign a value to a variable on declaration, use:
  -- 
  -- >>> declare $ variable "x" Par Int =. int 1
  -- 
  -- Not to be confused with the equality operator, represented in haskelzinc by '=.='.
  (=.) :: a -> Expr -> Assigned a

instance Assignable [Char] where
  type Assigned [Char] = Item
  name =. e = Assign name $ AnnExpr e []

instance Assignable Declaration where
  type Assigned Declaration = Declaration
  (Declaration ds ans _) =. e = Declaration ds ans (Just (AnnExpr e []))

-- | Used to represent declaration items of MiniZinc. These are variable, function, 
-- predicate, test and annotation declaration items.
declare :: Declaration -> Item
declare = Declare

declareOnly :: DeclarationSignature -> Declaration
declareOnly ds = Declaration ds [] Nothing

-- | Used together with 'declare' to represent a variable declaration item.
-- 
-- >>> declare $ variable Dec Int "x"
variable :: Inst -> Type -> Ident -> Declaration
variable i t s = declareOnly $ Variable (i, t, s)

-- | Used together with 'declare' to represent a predicate declaration item.
-- 
-- >>> declare $ predicate "even"[(Dec, Int, "x")] =. var "x" `_mod_` int 2
-- predicate even(var int: x) = x mod 2;
predicate :: Ident -> [Param] -> Declaration
predicate name ps = declareOnly $ Predicate name ps

-- | Used together with 'declare' to represent a test declaration item.
test :: Ident -> [Param] -> Declaration
test name ps = declareOnly $ Test name ps

-- | Used together with 'declare' to represent a function declaration item.
-- 
-- >>> declare $ function Dec Int "addFive" [(Dec, Int, "x")] =. var "x" +. int 5
-- function var int: addFive(var int: x) = x + 5;
function :: Inst -> Type -> Ident -> [Param] -> Declaration
function i t s ps = declareOnly $ Function (i, t, s) ps

-- | Used together with 'declare' to represent an annotation declaration item.
annotation :: Ident -> [Param] -> Declaration
annotation i ps = declareOnly $ Annotation' i ps

-- Expressions (plain)

__ :: Expr
__ = AnonVar

-- | Used when refering to an already defined variable.
-- 
-- Example:
--
-- >>> constraint $ var "x" !=. int 1
-- constraint x != 1;
var :: Ident -> Expr
var = Var

-- | MiniZinc boolean constant @true@.
true :: Expr
true = BConst True

-- | MiniZinc boolean constant @false@.
false :: Expr
false = BConst False

-- | Used to represent a MiniZinc integer constant.
-- Example:
--
-- >>> constraint $ var "x" !=. int 1
-- constraint x != 1;
int :: Int -> Expr
int = IConst

-- | Used to represent a MiniZinc float constant.
float :: Float -> Expr
float = FConst

-- | Used to represent a MiniZinc string constant.
string :: String -> Expr
string = SConst

-- boolSet :: [Bool] -> Expr
-- boolSet = SetLit . (map BConst)

-- | Used to represent a MiniZinc set of integers.
intSet :: [Int] -> Expr
intSet = mapSet IConst

-- | Used to represent a MiniZinc set of floats.
floatSet :: [Float] -> Expr
floatSet = mapSet FConst

-- | Used to represent a MiniZinc set of strings.
stringSet :: [String] -> Expr
stringSet = mapSet SConst

-- | Used to represent a MiniZinc set. In @mapSet f ls@, the elements of the MiniZinc 
-- set are represented by the resulting 'Expr's after applying @f@ on the elements of 
-- @ls@.
mapSet :: (a -> Expr) -> [a] -> Expr
mapSet f = SetLit . map f

-- | prop> set = SetLit
set :: [Expr] -> Expr
set = SetLit

-- | Used to represent a MiniZinc array of booleans.
boolArray :: [Bool] -> Expr
boolArray = mapArray BConst

-- | Used to represent a MiniZinc array of integers.
intArray :: [Int] -> Expr
intArray = mapArray IConst

-- | Used to represent a MiniZinc array of floats.
floatArray :: [Float] -> Expr
floatArray = mapArray FConst

-- | Used to represent a MiniZinc array of strings.
stringArray :: [String] -> Expr
stringArray = mapArray SConst

-- | Used to represent a 2-dimensional MiniZinc array of booleans.
boolArray2 :: [[Bool]] -> Expr
boolArray2 = mapArray2 BConst

-- | Used to represent a 2-dimensional MiniZinc array of integers.
intArray2 :: [[Int]] -> Expr
intArray2 = mapArray2 IConst

-- | Used to represent a 2-dimensional MiniZinc array of floats.
floatArray2 :: [[Float]] -> Expr
floatArray2 = mapArray2 FConst

-- | Used to represent a 2-dimensional MiniZinc array of strings.
stringArray2 :: [[String]] -> Expr
stringArray2 = mapArray2 SConst

-- | Represents a one-dimensional MiniZinc array by mapping.
mapArray :: (a -> Expr) -> [a] -> Expr
mapArray f = ArrayLit . map f

-- | @mapArray2 f lss@ represents a two-dimensional MiniZinc array by mapping @f@ to all
-- elements of all lists in @lss@.
mapArray2 :: (a -> Expr) -> [[a]] -> Expr
mapArray2 f = ArrayLit2D . (map (map f))

-- | prop> array = ArrayLit
array :: [Expr] -> Expr
array = ArrayLit

-- | prop> array2 = ArrayLit2
array2 :: [[Expr]] -> Expr
array2 = ArrayLit2D

-- Comprehension

infix 2 #/., #|.
-- | Creates the representation of a MiniZinc set comprehension. In @expr #/. cts@, 
-- @expr@ represents the head expression of the set comprehension and @cts@ is a list of 
-- its generator expressions' representations.
-- 
-- Example: 
-- 
-- >>> int 2 *. var "i" #/. [["i"] @@ int 0 ... int 5]
-- {2 * i | i in 0 .. 5}
(#/.) :: Expr -> [CompTail] -> Expr
e #/. cts = SetComp e (mergeCompTails cts)

-- | Similar to '#/.' for array comprehensions.
(#|.) :: Expr -> [CompTail] -> Expr
e #|. cts = ArrayComp e (mergeCompTails cts)

infix 9 !.
-- | Represents a MiniZinc array access.
-- 
-- Exaamples:
-- 
-- >>> "array"!.[int 1]
-- array[1]
-- 
-- >>> "matrix"!.[var "i", var "j"]
-- matrix[i,j]
(!.) :: Ident  -- ^ Array's name
     -> [Expr] -- ^ Indexes of the desired element
     -> Expr
(!.) = ArrayElem

-- | Used to construct the representation of a comprehension tail with a single generator 
-- expression. See the example in the documentation for '#/.'.
infix 5 @@
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

infix 4 `where_`
-- | Adds a representation for a MiniZinc @where@ clause in a generator expression.
--
-- Example:
--
-- >>> var "i" *. var "j" #/. [["i", "j"] @@ int 0 ... int 5 `where_` (var "i" !=. var "j")]
-- {i * j | i, j in 0 .. 5 where i != j}
where_ :: CompTail -> Expr -> CompTail
where_ (gs, _) e = (gs, Just e)

-- Generator calls
-- CompTail list makes no sense to be empty
-- | Used for the representation of a generator call.
--
-- Examples:
--
-- >>> forall [["i"] @@ var "S", ["j"] @@ var "S"] "sum" ("x"!.[var"i", var "j"]) =.= var "y"
-- sum(i in S, j in S) (x[i, j]) = y
--
-- >>> forall [["c"] @@ var "C"] "forall" (
-- >>>     forall [["s"] @@ var "S"] "sum" (mz_bool2int["bs"!.[var "s"] =.= var "c"])
-- >>> =.= "result"!.[var "c"])
-- forall(c in C) (sum(s in S) (bool2int(bs[s] = c)) = result[c])

forall :: [CompTail] -- ^ Generator expressions' representation
       -> Ident      -- ^ The name of the called operation
       -> Expr       -- ^ The head expression of the underlying array comprehension
       -> Expr
forall cts name e = GenCall name (mergeCompTails cts) e

-- Constrained types
-- | Represents a constrained type defined by a set parameter.
--
-- Example:
--
-- >>> declare $ variable Dec Int "one2three" =. intSet [1, 2, 3]
-- var int: one2three = {1, 2, 3};
-- 
-- >>> declare $ variable Dec (ctvar "one2three") "x"
-- var one2three: x;
ctvar :: Ident -> Type
ctvar = CT . var

-- | Represents a type variable.
($$) :: Ident -> Type
($$) = VarType

-- Auxiliary types for if-then-else expressions

-- | Used together with 'then_' and 'elseif_' \/ 'else_' to represent an if-then-else 
-- MiniZinc expression. In case of multiple alternatives, use 'elseif_', but the last 
-- alternative should be represented with the use of 'else_'.
-- 
-- Example:
--
-- >>> if_ true `then_` int 1 `else_` int 0
-- if true then 1 else 0 endif;
if_ :: Expr -> (Expr -> [(Expr, Expr)])
if_ e = \e1 -> [(e, e1)]

-- | cf. 'if_'
then_ :: (Expr -> [(Expr, Expr)]) -> Expr -> [(Expr, Expr)]
then_ f e = f e

-- | cf. 'if_'
elseif_ :: [(Expr, Expr)] -> Expr -> (Expr -> [(Expr, Expr)])
elseif_ es e = \e1 -> es ++ [(e, e1)]

-- | cf. 'if_'
else_ :: [(Expr, Expr)] -> Expr -> Expr
else_ = ITE

-- Let expressions
let_ = Let

-- Annotations
infixl 4 |:
class Annotatable a where
  -- | Adds a representation of an annotation to components that can be annotated.
  (|:) :: a -> Annotation -> a
  -- | Returns false if the given argument has an empty list of 'Annotation's, true otherwise.
  isAnnotated :: a -> Bool

instance Annotatable AnnExpr where
  (AnnExpr e a1) |: a2 = AnnExpr e (a1 ++ [a2])
  
  isAnnotated (AnnExpr _ []) = False
  isAnnotated _              = True

instance Annotatable Declaration where
  (Declaration ds a1 me) |: a2 = Declaration ds (a1 ++ [a2]) me
  
  isAnnotated (Declaration _ [] _) = False
  isAnnotated _                    = True

instance Annotatable Solve where
  (Satisfy a1)    |: a2 = Satisfy (a1 ++ [a2])
  (Minimize a1 e) |: a2 = Minimize (a1 ++ [a2]) e
  (Maximize a1 e) |: a2 = Maximize (a1 ++ [a2]) e
  
  isAnnotated (Satisfy [])     = False
  isAnnotated (Minimize [] _) = False
  isAnnotated (Maximize [] _) = False
  isAnnotated _                = True