{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Interfaces.MZAST where

import Interfaces.MZASTBase
import Interfaces.MZPrinter

-- Items'

newtype EmptyItem'    = EmptyItem' ()
newtype Comment'      = Comment' String
newtype Include'      = Include' String
newtype Declare'      = Declare' Declaration
newtype Assign'       = Assign' (Ident, Expr)
newtype Constraint'   = Constraint' Expr
data    Solve'        = Solve' [Annotation] Solve
newtype Output'       = Output' NakedExpr

emptyLine :: EmptyItem'
emptyLine = EmptyItem' ()

comment :: String -> Comment'
comment = Comment'

(%%) :: String -> Comment'
(%%) = comment

include :: String -> Include'
include = Include'

output :: NakedExpr -> Output'
output = Output'

(|:) :: (Item' a) => a -> [Item] -> [Item]
v |: is = toItem v : is

-- Internal

class Item' a where
  toItem :: a -> Item

instance Item' EmptyItem' where
  toItem x = Empty
instance Item' Comment' where
  toItem (Comment' text) = Comment text
instance Item' Include' where
  toItem (Include' file) = Include file
instance Item' Output' where
  toItem (Output' e) = Output e

-- Expressions (plain and annotated)

class Expression a where
  toExpression :: a -> Expr

instance Expression NakedExpr where
  toExpression e = Expr e []

instance Expression Expr where
  toExpression = id

class NakedExpression a where
  toNExpr :: a -> NakedExpr

instance NakedExpression NakedExpr where
  toNExpr = id

instance NakedExpression Int where
  toNExpr = IConst

instance NakedExpression Bool where
  toNExpr = BConst

instance NakedExpression Float where
  toNExpr = FConst

instance NakedExpression [Char] where
  toNExpr = SConst

instance NakedExpression a => NakedExpression (Op, a) where
  toNExpr (op, e) = U op (toNExpr e)

instance (NakedExpression a, NakedExpression b) => NakedExpression (a, Op, b) where
  toNExpr (e1, op, e2) = Bi op (toNExpr e1) (toNExpr e2)

instance NakedExpression IF_THEN_ELSE where
  toNExpr (ELSE e e1 e2) = ITE [(e, e1)] e2

set :: (NakedExpression a) => [a] -> NakedExpr
set = SetLit . (map toNExpr)

array :: (NakedExpression a) => [a] -> NakedExpr
array = ArrayLit . (map toNExpr)

(\\) :: Ident -> [NakedExpr] -> NakedExpr
name \\ is = ArrayElem name is

-- Auxiliary types for if-then-else expressions

data IF_THEN_ELSE = ELSE NakedExpr NakedExpr NakedExpr
data ELSEIF = ELSEIF [(NakedExpr, NakedExpr)] NakedExpr

if' :: NakedExpr -> (NakedExpr -> NakedExpr -> IF_THEN_ELSE)
if' e = \e1 e2 -> ELSE e e1 e2

then' :: (NakedExpr -> NakedExpr -> IF_THEN_ELSE) -> NakedExpr -> (NakedExpr -> IF_THEN_ELSE)
then' f e = f e

else' :: (NakedExpr -> IF_THEN_ELSE) -> NakedExpr -> IF_THEN_ELSE
else' f e = f e

-- Assignments (plain and when declaring sth)
class Assignable a where
  type Assigned a
  (=.) :: (Expression b) => a -> b -> Assigned a

instance Assignable [Char] where
  type Assigned [Char] = Assign'
  name =. expr = Assign' (name, toExpression expr)

instance Assignable DeclarationSignature where
  type Assigned DeclarationSignature = Declare'
  ds =. expr = Declare' $ Declaration ds [] (Just (toExpression expr))

-- Annotations 
class WithAnnotation a where
  type Annotated a
  (|>) :: a -> [Annotation] -> Annotated a

instance WithAnnotation Declaration where
  type Annotated Declaration = Declare'
  (Declaration ds _ me) |> ans = Declare' $ Declaration ds ans me

instance WithAnnotation Expr where
  type Annotated Expr = Expr
  (Expr e _) |> ans = Expr e ans

instance WithAnnotation NakedExpr where
  type Annotated NakedExpr = Expr
  e |> ans = Expr e ans

instance WithAnnotation Assign' where
  type Annotated Assign' = Assign'
  (Assign' (name, body)) |> ans = Assign' (name, body |> ans)

instance WithAnnotation Constraint' where
  type Annotated Constraint' = Constraint'
  (Constraint' e) |> ans = Constraint' (e |> ans)
  
instance WithAnnotation Solve' where
  type Annotated Solve' = Solve'
  (Solve' _ s) |> ans = Solve' ans s