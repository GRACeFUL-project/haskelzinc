{-|
Module      : MZAST
Description : MiniZinc abstract syntax tree
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module provides an interface of the MiniZinc 2.1 language in Haskell through the definition of an abstract syntax tree of the MiniZinc language.
With the use of this module, one can represent MiniZinc models in Haskell code. The abstract syntax tree is based on 
<http://www.minizinc.org/doc-lib/minizinc-spec.pdf the MiniZinc 2.1 spesification>. 

However, the module does not check semantical correctness of the represented model.
For example, it does not detect typos in the use of previously declared identifiers.

=== Featrues not supported yet
 - Annotations
-}

module Interfaces.MZAST (
  MZModel,
  Item(..),
  Expr(..),
  NakedExpr(..),
  Type(..),
  Op(..),
  prefbop,
  Func(..),
  Annotation(..),
  Inst(..),
  Solve(..),
  CompTail,
  Generator,
  --TypeInst,
  Param,
  Ident,
  Filename
) where 

-- | An abbreviation for the type of a represented MiniZinc model.
type MZModel = [Item]

{-|
  The type of a MiniZinc's top-level program item representation. MiniZinc defines 8 kinds of
  items. This module defines a representation for 12 kinds of items. The additional 4 come from 
  representing MiniZinc commented lines and empty lines as items, and from using 3 distinct Item
  constructors for representing user defined calls (predicates, tests and functions).
-}
data Item 
  -- | Commented line
  = Comment String
  -- | Include item
  | Include Filename
  -- | Variable declaration item.
  -- The value @Declare i t name maybe_exp@ represents the declaration a variable named @name@ of type @t@ and inst @i@. 
  -- Use @Just expression@ in place of @maybe_exp@ to represent the value that initializes the declared 
  -- variable. Use @Nothing@ in place of @maybe_exp@ to represent a variable declaration without initialization.
  | Declare Param [Annotation] (Maybe NakedExpr)
  -- | Assignment item. @Assign name exp@ represents the assignment of @exp@ to the variable @name@.
  | Assign Ident NakedExpr
  -- | Constraint item
  | Constraint Expr
  -- | Solve item
  | Solve [Annotation] Solve
  -- | Output item. The use of this item might cause errors in parsing the solution(s) of the model.
  -- Recommended use for testing purposes only.
  | Output NakedExpr
  -- | User-defined predicate. @Pred name args exp@ represents the MiniZinc definition of a predicate 
  -- called @name@, the parameters of which are the elements of the @args@ list. @exp@ represents the
  -- optional body of the predicate.
  | Pred Ident [Param] [Annotation] (Maybe NakedExpr)
  -- | User-defined test. Syntax similar to the @Pred@ constructor.
  | Test Ident [Param] [Annotation] (Maybe NakedExpr)
  -- | User-defined function. Syntax similar to @Pred@ and @Test@ constructors. The additional @Param@
  -- represents the type and inst of the returning value of the function, along with the name of the 
  -- function.
  | Function Param [Param] [Annotation] (Maybe NakedExpr)
  -- | Represents the declaration of a user defined annotation. The first argument represents the name of the annotation and the second encodes its arguments.
  | AnnotDec Ident [Param]
  -- | Represents an empty line in the MiniZinc model.
  | Empty        
  deriving Eq

-- Represents a MiniZinc expression (first argument) annotated with the annotations contained in the list of the second argument.
data Expr = Expr NakedExpr [Annotation]
  deriving (Eq)

-- | The type of a MiniZinc expression's representation.
data NakedExpr
  -- | Represents the MiniZinc special variable @_@.
  = AnonVar
  -- | A MiniZinc variable
  | Var Ident
  -- | MiniZinc boolean value
  | BConst Bool
  -- | MiniZinc integer value
  | IConst Int
  -- | MiniZinc float value
  | FConst Float
  -- | MiniZinc string value
  | SConst String
  -- @Range a b@ represents the set defined by @a .. b@ in MiniZinc.
  | Range NakedExpr NakedExpr
  -- | @SetLit literals@ translates to a MiniZinc set the elements of which are the represented expressions in
  -- the @literals@ list.
  | SetLit [NakedExpr]
  -- | MiniZinc set comprehension. The first argument of the constructor represents the head
  -- expression of the comprehension, while the second represents the comprehension tail.
  | SetComp NakedExpr CompTail
  -- | MiniZinc 1-dimensional arrays defined with literals, similar to the @SetLit@ constructor.
  | ArrayLit [NakedExpr]
  -- | MiniZinc 2-dimensional arrays defined with literals
  | ArrayLit2D [[NakedExpr]]
  -- | MiniZinc array comprehension. Syntax similar to @SetComp@ constructor.
  | ArrayComp NakedExpr CompTail
  -- | Represents an array element. In @ArrayElem name is@, the argument @name@ is the identifier of the array and @is@ is
  -- the list of indexes that specify the desired element. The length of @is@ must be equal to the number of 
  -- dimensions of the array.
  | ArrayElem Ident [NakedExpr]
  -- | @Bi op exp1 exp2@ represents the MiniZinc expression that applies the binary operator @op@ on @exp1@ and @exp2@.
  | Bi Op NakedExpr NakedExpr
  -- | @U op exp1@ represents the MiniZinc expression that applies the unary operator @op@ on @exp1@.
  | U Op NakedExpr
  -- | @Call name args@ represents a call to the function or test @name@ on arguments @args@.
  | Call Func [NakedExpr]
  -- | The if-then-else conditional. If the first argument of the constructor is an empty list, the translation to MiniZinc will fail.
  -- @ITE [(cond, expr1)] expr2@, where the list is a singleton, translates to @if cond then exp1 else exp2 endif@.
  -- If the list contains more than one pairs, then the corresponding @elseif-then@ pairs are inserted before the final @else@ expression.
  | ITE [(NakedExpr, NakedExpr)] NakedExpr
  -- | @let-in@ expression. In @Let items expr@, the elements of @items@ represent the bindings in the @expr@ expression. Although @items@
  -- is of type @[Items]@, only @Item@ values constructed by @Declare@ and @Constraint@ will translate to a syntactically correct
  -- MiniZinc let expression.
  | Let [Item] NakedExpr
  -- | A generator call expression.
  | GenCall Func CompTail NakedExpr 
  deriving Eq

-- | The type of a MiniZinc's type representation.
data Type
  = Bool
  | Int
  | Float
  | String
  -- | @Set t@ translates to @set of t@.
  | Set Type
  -- | @Array ts ti@ translates to @array [ts] of ti@. 
  | Array [Type] TypeInst
  -- | The list type
  | List TypeInst
  -- | Option type
  | Opt Type
  -- | Annotation type
  | Ann
  -- | A constrained type using the integer range. @Interval a b@ translates to @a .. b@.
  | Interval NakedExpr NakedExpr
  -- | A constrained type using set literals.
  | Elems [NakedExpr]
  -- | A constrained type using a previously defined set parameter.
  | AOS Ident
  -- | Type variable
  | VarType Ident
  deriving Eq

-- | Represents an operator name in MiniZinc.
newtype Op = Op String
  deriving (Eq)

-- | The type of a MiniZinc's function, test or predicate representation.
data Func
  = CName Ident  -- ^ Represents a MiniZinc call (function, predicate or test). The @Ident@ argument is the name of the call.
  | PrefBop Op
  deriving Eq
  
data Annotation = AName Ident [NakedExpr]
  deriving (Eq)

-- | Prefix notation of a MiniZinc built-in binary operator.
prefbop :: Op -> Func
prefbop = PrefBop

-- | The type of a MiniZinc instantiation representation.
data Inst
  = Par -- ^ A @par@ instantiation in MiniZinc.
  | Dec -- ^ A @var@ instantiation in MiniZinc.
  deriving Eq

-- | The type for representing the three different kinds of solve items. 
data Solve
  = Satisfy
  | Minimize Expr
  | Maximize Expr
  deriving Eq

type CompTail = ([Generator], Maybe NakedExpr)

type Generator = ([Ident], NakedExpr)
 
type TypeInst = (Inst, Type)

type Param = (Inst, Type, Ident)

type Ident = String

type Filename = String
