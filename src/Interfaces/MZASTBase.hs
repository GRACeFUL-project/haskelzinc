{-|
Module      : MZASTBase
Description : MiniZinc abstract syntax tree
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental


This module provides an interface for the MiniZinc 2.1 language.
With the use of this module, one can represent MiniZinc models in Haskell code. The 
syntax is based on 
<http://www.minizinc.org/doc-lib/minizinc-spec.pdf the MiniZinc 2.1 spesification>. 
However, this module provides a low-level interface to the MiniZinc language. A more 
human friendly interface is provided in "Interfaces.MZAST".

Enumerated types are not supported yet.
-}

module Interfaces.MZASTBase (
  MZModel,
  -- * Items representation
  Item(..),
  Declaration(..),
  DeclarationSignature(..),
  Solve(..),
  Inst(..),
  Type(..),
  -- * Expressions representation
  AnnExpr(..),
  Expr(..),
  stripExprOff,
  toSimpleExpr,
  Op(..),
  GArguments(..),
  Annotation(..),
  -- * Abbreviations
  CompTail,
  Generator,
  Param,
  Ident,
  Filename
) where 

-- | An abbreviation for the type of a represented MiniZinc model.
type MZModel = [Item]

-- | Represents MiniZinc items, the first-class entities of the MiniZinc language.
-- Correspondence between MiniZinc items and the constructors of 'Item' values is not
-- one-to-one:
-- 
-- * Variable, function, predicate, test and annotation declaration items are all 
--   represented with the 'Declare' constructor.
-- * Extra 'Comment' constructor for the representation of MiniZinc comments.
data Item 
  -- | Commented line
  = Comment String
  -- | Include item
  | Include Filename
  -- | A declaration item. Can represent a MiniZinc variable, predicate, test, function 
  -- or annotation declaration. This is specified by the constructor\'s argument.
  | Declare Declaration
  -- | Assignment item. @Assign name exp@ represents the assignment of @exp@ to the variable @name@.
  | Assign Ident AnnExpr
  -- | Constraint item
  | Constraint AnnExpr
  -- | Solve item
  | Solve Solve
  -- | Output item. The use of this item might cause errors in parsing the solution(s) 
  -- of the model. Recommended use for testing purposes only.
  | Output Expr
  {-
  -- | 'Empty' does not translate to a MiniZinc item. It only represents an empty line 
  -- in the MiniZinc code.
  | Empty
  -}
  deriving (Show, Eq)

-- | Represents a MiniZinc expression (first argument) annotated with the annotations 
-- contained in the list of the second argument.
data AnnExpr = AnnExpr Expr [Annotation]
  deriving (Show, Eq)

-- | Transforms an 'Expr' to an 'AnnExpr' with an empty list of annotations.
toSimpleExpr :: Expr -> AnnExpr
toSimpleExpr e = AnnExpr e []

-- | Takes an annotated expression and returns only the expression.
stripExprOff :: AnnExpr -> Expr
stripExprOff (AnnExpr e ans) = e

-- | Represents a complete variable, predicate, test or function declaration with a list
-- of annotations (possibly empty) and maybe a body.
data Declaration = Declaration DeclarationSignature [Annotation] (Maybe AnnExpr)
  deriving (Show, Eq)

-- | Used for the representation of the signature of a variable, function, predicate, test
-- or annotation declaration.
data DeclarationSignature = Variable Param 
                          | Predicate Ident [Param]
                          | Test Ident [Param]
                          | Function Param [Param]
                          | Annotation' Ident [Param]
  deriving (Show, Eq)

-- | The type of a MiniZinc expression's representation.
data Expr
  -- | Represents the MiniZinc special variable @_@.
  = AnonVar
  -- | A MiniZinc variable.
  | Var Ident
  -- | MiniZinc boolean value.
  | BConst Bool
  -- | MiniZinc integer value.
  | IConst Int
  -- | MiniZinc float value.
  | FConst Float
  -- | MiniZinc string value.
  | SConst String
  -- | @SetLit ls@ translates to a MiniZinc set the elements of which are the represented
  -- expressions in the @ls@ list.
  | SetLit [Expr]
  -- | MiniZinc set comprehension. The first argument of the constructor represents the
  -- head expression of the comprehension, while the second represents the comprehension 
  -- tail.
  | SetComp Expr CompTail
  -- | MiniZinc 1-dimensional arrays defined with literals, similar to the 'SetLit' 
  -- constructor.
  | ArrayLit [Expr]
  -- | MiniZinc 2-dimensional arrays defined with literals.
  | ArrayLit2D [[Expr]]
  -- | MiniZinc array comprehension. Syntax similar to 'SetComp' constructor.
  | ArrayComp Expr CompTail
  -- | Represents an array element. In @ArrayElem name js@, the argument @name@ is the 
  -- identifier of the array and @js@ is the list of indexes that specify the desired 
  -- element. The length of @js@ must be equal to the number of dimensions of the array.
  | ArrayElem Ident [Expr]
  -- | @Bi op exp1 exp2@ represents the MiniZinc expression of the binary operator @op@
  -- applied on @exp1@ and @exp2@.
  | Bi Op Expr Expr
  -- | @U op exp@ represents the MiniZinc application of the unary operator (represented
  -- by) @op@ on the expression (represented by) @exp@.
  | U Op Expr
  -- | @Call name args@ represents a call to the function, predicate or test @name@ on 
  -- arguments @args@. A call to an annotation is represented by the 'Annotation' type.
  | Call Ident [AnnExpr]
  -- | The if-then-else conditional. If the first argument of the constructor is an 
  -- empty list, the translation to MiniZinc will fail. @ITE [(cond, expr1)] expr2@,
  -- where the list is a singleton, translates to @if cond then exp1 else exp2 endif@.
  -- If the list contains more than one pairs, then the corresponding @elseif-then@
  -- pairs are inserted before the final @else@ expression.
  | ITE [(Expr, Expr)] Expr
  -- | @let-in@ expression. In @Let items expr@, the elements of @items@ represent the 
  -- bindings in the @expr@ expression. Although @items@ is of type @[Item]@, only 
  -- 'Item' values constructed by 'Declare' and 'Constraint' will translate to a 
  -- syntactically correct MiniZinc let expression.
  | Let [Item] Expr
  -- | A generator call expression.
  | GenCall Ident CompTail Expr 
  deriving (Show, Eq)

-- | The type of a MiniZinc's type representation.
data Type
  = Bool
  | Int
  | Float
  | String
  -- | @Set t@ translates to @set of t@.
  | Set Type
  -- | @Array ts inst ty@ translates to @array [ts] of inst ty@. 
  | Array [Type] Inst Type
  -- | The list type
  | List Inst Type
  -- | Option type
  | Opt Type
  -- | Annotation type
  | Ann
  -- | A constrainted type. The 'Expr' argument should represent a MiniZinc finite set.
  | CT Expr
  {-
  -- | A constrained type using the integer range. @Range a b@ translates to @a .. b@.
  | Range Expr Expr
  -- | A constrained type using set literals.
  | Elems [Expr]
  -- | A constrained type using a previously defined set parameter.
  | ACT Ident
  -}
  -- | Type variable
  | VarType Ident
  deriving (Show, Eq)

-- | Represents an operator name/symbol in MiniZinc.
newtype Op = Op Ident
  deriving (Show, Eq)

-- | Transforms an operator to a quoted operator (which, in MiniZinc admits prefix 
-- notation). One can represent the application of a quoted operator though the 'Call'
-- constructor of type 'Item'.
prefixOp :: Op -> Ident
prefixOp (Op op) = "`" ++ op ++ "`"

-- | Used in annotations' arguments, which can be either annotations or expressions.
data GArguments = A Annotation
                | E Expr
  deriving (Show, Eq)

-- | Represents a call to a MiniZinc annotation. First argument represents the 
-- annotation\'s name and second argument contains the annotation's arguments, if any.
data Annotation = Annotation Ident [GArguments]
  deriving (Show, Eq)

-- | The type of a MiniZinc instantiation representation.
data Inst
  = Par -- ^ A @par@ instantiation in MiniZinc.
  | Dec -- ^ A @var@ instantiation in MiniZinc.
  deriving (Show, Eq)

-- | The type for representing the three different kinds of solve items. 
data Solve
  = Satisfy [Annotation]
  | Minimize [Annotation] Expr
  | Maximize [Annotation] Expr
  deriving (Show, Eq)

type CompTail = ([Generator], Maybe Expr)

type Generator = ([Ident], Expr)
 
type TypeInst = (Inst, Type)

type Param = (Inst, Type, Ident)

type Ident = String

type Filename = String