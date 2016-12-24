module Interfaces.MZAST(
  -- * Items
  (|+),
  -- ** Declaration items
  var, predicate, test, function, annotation, (.=), (|>),
  -- ** Other items
  constraint, solve, (=.), (##), include, output
  -- * Expressions
) where

import Interfaces.MZASTBase
import Interfaces.MZPrinter

declare :: DeclarationSignature -> Declaration
declare ds = Declaration ds [] Nothing

-- Declaring a variable
var :: (Inst, Type) -> Ident -> Declaration
var (i, t) name = declare $ Variable (i, t, name)

-- Declaring a predicate
predicate :: Ident -> [Param] -> Declaration
predicate name args = declare (Predicate name args)

-- Declaring a test
test :: Ident -> [Param] -> Declaration
test name args = declare (Test name args)

-- Declaring a function
function :: (Inst, Type) -> Ident -> [Param] -> Declaration
function (i, t) name args = declare (Function (i, t, name) args)

-- Declaring an annotation
annotation :: Ident -> [Param] -> Declaration
annotation name args = declare (Annotation' name args)

-- Building up a model
(|+) :: MZModel -> Item -> MZModel
(|+) = flip (:)

-- Assignment when declaring
(.=) :: Declaration -> NakedExpr -> Declaration
(Declaration ds ans _) .= e = Declaration ds ans (Just e)

-- Annotation
class WithAnnotation a where
  (|>) :: a -> [Annotation] -> a

instance WithAnnotation Declaration where
  (Declaration ds _ me) |> ans = Declaration ds ans me

instance WithAnnotation Expr where
  (Expr ne _) |> ans = Expr ne ans

-- Only annotating a declaration, a constraint or a solve item makes sense.
instance WithAnnotation Item where
  (Declare d)    |> ans = Declare (d |> ans)
  (Solve _ s)    |> ans = Solve ans s
  (Constraint e) |> ans = Constraint (e |> ans)
  other          |> ans = other

-- Other items
(##) :: String -> Item
(##) r = Comment r

-- Posting a constraint
constraint :: NakedExpr -> Item
constraint e = Constraint (Expr e [])

include :: String -> Item
include = Include

output :: NakedExpr -> Item
output = Output

(=.) :: String -> NakedExpr -> Item
name =. e = Assign name e

solve :: Solve -> Item
solve = Solve []

nl :: Item
nl = Empty

-- Expressions (naked)
class ToExpr a where
  toExpr :: a -> NakedExpr

instance ToExpr (Char) where
  toExpr '_' = AnonVar

instance ToExpr Bool where
  toExpr = BConst

instance ToExpr Int where
  toExpr = IConst

instance ToExpr Float where
  toExpr = FConst

instance Callable where
  toExpr = Call