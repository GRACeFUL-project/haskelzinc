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
-}

module Interfaces.MZASTBase (
  MZModel,
  Item(..),
  DeclarationSignature(..),
  Declaration(..),
  Expr(..),
  NakedExpr(..),
  Type(..),
  Op(..),
  Callable(..),
  Annotation(..),
  Inst(..),
  Solve(..),
  CompTail,
  Generator,
  Param,
  Ident,
  Filename
) where 

-- | An abbreviation for the type of a represented MiniZinc model.
type MZModel = [Item]

data Item 
  -- | Commented line
  = Comment String
  -- | Include item
  | Include Filename
  -- | A declaration item. Can represent a MiniZinc variable, predicate, test, function or 
  -- annotation declaration. This is specified by the constructor's argument.
  | Declare Declaration
  -- | Assignment item. @Assign name exp@ represents the assignment of @exp@ to the variable @name@.
  | Assign Ident Expr
  -- | Constraint item
  | Constraint Expr
  -- | Solve item
  | Solve [Annotation] Solve
  -- | Output item. The use of this item might cause errors in parsing the solution(s) of the model.
  -- Recommended use for testing purposes only.
  | Output NakedExpr
  -- | Represents an empty line in the MiniZinc model.
  | Empty        
  deriving (Show, Eq)

-- Represents a MiniZinc expression (first argument) annotated with the annotations contained in the list of the second argument.
data Expr = Expr NakedExpr [Annotation]
  deriving (Show, Eq)

-- | Completes a declaration with a list of annotations (possibly empty) and maybe a body.
data Declaration = Declaration DeclarationSignature [Annotation] (Maybe Expr)
  deriving (Show, Eq)

-- | Specifies wether the declaration is a variable, predicate, test, function or annotation
-- declaration. For each case, the arguments hold the signature of the declaration.
data DeclarationSignature = Variable Param 
                          | Predicate Ident [Param]
                          | Test Ident [Param]
                          | Function Param [Param]
                          | Annotation' Ident [Param]
  deriving (Show, Eq)

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
  | Call Callable
  -- | The if-then-else conditional. If the first argument of the constructor is an empty list, the translation to MiniZinc will fail.
  -- @ITE [(cond, expr1)] expr2@, where the list is a singleton, translates to @if cond then exp1 else exp2 endif@.
  -- If the list contains more than one pairs, then the corresponding @elseif-then@ pairs are inserted before the final @else@ expression.
  | ITE [(NakedExpr, NakedExpr)] NakedExpr
  -- | @let-in@ expression. In @Let items expr@, the elements of @items@ represent the bindings in the @expr@ expression. Although @items@
  -- is of type @[Items]@, only @Item@ values constructed by @Declare@ and @Constraint@ will translate to a syntactically correct
  -- MiniZinc let expression.
  | Let [Item] NakedExpr
  -- | A generator call expression.
  | GenCall Ident CompTail NakedExpr 
  deriving (Show, Eq)

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
  -- | A constrained type using the integer range. @Range a b@ translates to @a .. b@.
  | Range NakedExpr NakedExpr
  -- | A constrained type using set literals.
  | Elems [NakedExpr]
  -- | A constrained type using a previously defined set parameter.
  | AOS Ident
  -- | Type variable
  | VarType Ident
  deriving (Show, Eq)

-- | Represents an operator name in MiniZinc.
newtype Op = Op String
  deriving (Show, Eq)

-- | Represents a call to a MiniZinc function, test, predicate or annotation.
data Callable = Callable Ident [Expr] -- ^ Name given by the user
--  | PrefBop Op [NakedExpr]  -- ^ Prefix syntax of given (representation of) MiniZinc operator.
  deriving (Show, Eq)

prefixOp :: Op -> Ident
prefixOp (Op name) = "`" ++ name ++ "`"

-- | Represents the name of a MiniZinc annotation.  
data Annotation = Annotation Ident [NakedExpr]
  deriving (Show, Eq)

-- | The type of a MiniZinc instantiation representation.
data Inst
  = Par -- ^ A @par@ instantiation in MiniZinc.
  | Dec -- ^ A @var@ instantiation in MiniZinc.
  deriving (Show, Eq)

-- | The type for representing the three different kinds of solve items. 
data Solve
  = Satisfy
  | Minimize Expr
  | Maximize Expr
  deriving (Show, Eq)

type CompTail = ([Generator], Maybe NakedExpr)

type Generator = ([Ident], NakedExpr)
 
type TypeInst = (Inst, Type)

type Param = (Inst, Type, Ident)

type Ident = String

type Filename = String
