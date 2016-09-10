{-|
Module      : MZAST
Description : MiniZinc abstract syntax tree
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module provides an interface of the MiniZinc 2.0 language in Haskell through the definition of an abstract syntax tree of the MiniZinc language.
With the use of this module, one can represent MiniZinc models in Haskell code. The abstract syntax tree is based on 
<http://www.minizinc.org/2.0/doc-lib/minizinc-spec.pdf the MiniZinc 2.0 spesification>. 

However, the module does not check semantical correctness of the represented model.
For example, it does not detect typos in the use of previously declared identifiers.

=== Featrues not supported yet
 - Annotations
-}

module Interfaces.MZAST (
  MZModel,
  Item(..),
  Expr(..),
  VarType(..),
  -- * MiniZinc operators
  Bop(..),
  Uop(..),
  -- * MiniZinc built-in calls
  userD, prefbop,
  -- ** Arithmetic calls
  mz_abs, mz_sum, mz_max, mz_min, mz_pow, mz_sqrt,
  mz_exp, mz_ln, mz_log, mz_log10, mz_log2,
  mz_sin, mz_cos, mz_tan, mz_sinh, mz_cosh, mz_tanh,
  mz_asin, mz_acos, mz_atan, mz_asinh, mz_acosh, mz_atanh,
  -- ** Logical calls
  mz_forall, mz_xorall,
  -- ** String calls
  mz_show, mz_show_int, mz_show_float, mz_concat, mz_join,
  -- ** Set calls
  mz_card, mz_array_union,
  -- ** Array calls
  mz_length, mz_index_set, mz_index_set_1of2, mz_index_set_2of2,
  mz_array1d, mz_array2d, mz_array3d, mz_array4d, mz_array5d, mz_array6d,
  -- ** Option type calls
  mz_occurs, mz_absent, mz_deopt,
  -- ** Coercion calls
  mz_ceil, mz_floor, mz_round,
  mz_bool2int, mz_int2float, mz_set2array,
  -- ** Bound and domain calls
  mz_lb, mz_ub, mz_lb_array, mz_ub_array, mz_dom, mz_dom_array, mz_dom_size,
  -- ** Other calls
  mz_assert, mz_abort, mz_trace, mz_fix, mz_is_fixed,
  Func(..),
  Inst(..),
  Solve(..),
  CompTail,
  Generator,
  TypeInst,
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
  | Declare TypeInst Ident (Maybe Expr)
  -- | Assignment item. @Assign name exp@ represents the assignment of @exp@ to the variable @name@.
  | Assign Ident Expr
  -- | Constraint item
  | Constraint Expr
  -- | Solve item
  | Solve Solve
  -- | Output item. The use of this item might cause errors in parsing the solution(s) of the model.
  -- Recommended use for testing purposes only.
  | Output Expr
  -- | User-defined predicate. @Pred name args exp@ represents the MiniZinc definition of a predicate 
  -- called @name@, the parameters of which are the elements of the @args@ list. @exp@ represents the
  -- optional body of the predicate.
  | Pred Ident [Param] (Maybe Expr)
  -- | User-defined test. Syntax similar to the @Pred@ constructor.
  | Test Ident [Param] (Maybe Expr)
  -- | User-defined function. Syntax similar to @Pred@ and @Test@ constructors. The additional @TypeInst@
  -- represents the type of the returning value of the function and the inst of the function.
  | Function TypeInst Ident [Param] (Maybe Expr)
  -- | Annotation item. Use of annotations is not supported yet.
  | Annotation
  -- | Represents an empty line in the MiniZinc model.
  | Empty        
  deriving Eq

-- | The type of a MiniZinc expression's representation.
data Expr
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
  -- | MiniZinc arrays constructed with the MiniZinc @..@ operator.
  -- @Interval a b@ translates to @[a .. b]@.
  | Interval Expr Expr
  -- | @SetLit literals@ translates to a MiniZinc set the elements of which are the represented expressions in
  -- the @literals@ list.
  | SetLit [Expr]
  -- | MiniZinc set comprehension. The first argument of the constructor represents the head
  -- expression of the comprehension, while the second represents the comprehension tail.
  | SetComp Expr CompTail
  -- | MiniZinc 1-dimensional arrays defined with literals, similar to the @SetLit@ constructor.
  | ArrayLit [Expr]
  -- | MiniZinc 2-dimensional arrays defined with literals
  | ArrayLit2D [[Expr]]
  -- | MiniZinc array comprehension. Syntax similar to @SetComp@ constructor.
  | ArrayComp Expr CompTail
  -- | Represents an array element. In @ArrayElem name is@, the argument @name@ is the identifier of the array and @is@ is
  -- the list of indexes that specify the desired element. The length of @is@ must be equal to the number of 
  -- dimensions of the array.
  | ArrayElem Ident [Expr]
  -- | @Bi op exp1 exp2@ represents the MiniZinc expression that applies the binary operator @op@ on @exp1@ and @exp2@.
  | Bi Bop Expr Expr
  -- | @U op exp1@ represents the MiniZinc expression that applies the unary operator @op@ on @exp1@.
  | U Uop Expr
  -- | @Call name args@ represents a call to the function or test @name@ on arguments @args@.
  | Call Func [Expr]
  -- | The if-then-else conditional. If the first argument of the constructor is an empty list, the translation to MiniZinc will fail.
  -- @ITE [(cond, expr1)] expr2@, where the list is a singleton, translates to @if cond then exp1 else exp2 endif@.
  -- If the list contains more than one pairs, then the corresponding @elseif-then@ pairs are inserted before the final @else@ expression.
  | ITE [(Expr, Expr)] Expr
  -- | @let-in@ expression. In @Let items expr@, the elements of @items@ represent the bindings in the @expr@ expression. Although @items@
  -- is of type @[Items]@, only @Item@ values constructed by @Declare@ and @Constraint@ will translate to a syntactically correct
  -- MiniZinc let expression.
  | Let [Item] Expr
  -- | A generator call expression.
  | GenCall Func CompTail Expr 
  deriving Eq

-- | The type of a MiniZinc's type representation.
data VarType
  = Bool
  | Int
  | Float
  | String
  -- | @Set t@ translates to @set of t@.
  | Set VarType
  -- | @Array ts ti@ translates to @array [ts] of ti@. 
  | Array [VarType] TypeInst
  -- | The list type
  | List TypeInst
  -- | Option type
  | Opt VarType
  -- | A constrained type using the integer range. @Range a b@ translates to @a .. b@.
  | Range Expr Expr
  -- | A constrained type using set literals.
  | Elems [Expr]
  -- | A constrained type using a previously defined set parameter.
  | AOS Ident
  | Any
  deriving Eq

-- | The type of MiniZinc binary operators' representation. Next to each constructor is indicated the operator it represents.
data Bop 
  -- Comparison
  = Gt  -- ^ @>@
  | Lt  -- ^ @<@
  | Gte -- ^ @>=@
  | Lte -- ^ @<=@
  | Eqq -- ^ @==@
  | Eq  -- ^ @=@
  | Neq -- ^ @!=@
  
  -- Arithmetic
  | BPlus   -- ^ @+@
  | BMinus  -- ^ @-@
  | Times   -- ^ @*@
  | Div     -- ^ @/@
  | IDiv    -- ^ @div@
  | Mod     -- ^ @mod@
  
  -- Logical
  | LRarrow -- ^ @\<-\>@
  | Rarrow  -- ^ @->@
  | Larrow  -- ^ @<-@
  | And     -- ^ @\/\\@
  | Or      -- ^ @\\\/@
  
  -- Sets
  | In      -- ^ @in@
  | Sub     -- ^ @subset@
  | Super   -- ^ @superset@
  | Union   -- ^ @union@
  | Inters  -- ^ @intersect@
  
  -- Arrays
  | Concat  -- ^ @++@
  
  -- Misc
  | Diff    -- ^ @diff@
  | SDiff   -- ^ @symdiff@
  | RangeOp -- ^ @..@
  | AsFunc Bop
  deriving Eq

-- | Represents MiniZinc unary operators. Next to each constructor is indicated the operator it represents.  
data Uop 
  = Not     -- ^ @not@
  | UPlus   -- ^ @+@
  | UMinus  -- ^ @-@
 deriving Eq

-- | The type of a MiniZinc's function, test or predicate representation.
data Func
  = CName Ident
  | PrefBop Bop
  deriving Eq

-- | User defined function, test or predicate in MiniZinc. The argument of this constructor
-- is the name of the function.
userD :: Ident -> Func
userD = CName

-- | Prefix notation of a MiniZinc built-in binary operator.
prefbop :: Bop -> Func
prefbop = PrefBop

mz_abort :: Func
mz_abort = userD "abort"

mz_abs :: Func
mz_abs = userD "abs"

mz_absent :: Func
mz_absent = userD "absent"

mz_acos :: Func
mz_acos = userD "acos"

mz_acosh :: Func
mz_acosh = userD "acosh"

mz_array1d :: Func
mz_array1d = userD "array1d"

mz_array2d :: Func
mz_array2d = userD "array2d"

mz_array3d :: Func
mz_array3d = userD "array3d"

mz_array4d :: Func
mz_array4d = userD "array4d"

mz_array5d :: Func
mz_array5d = userD "array5d"

mz_array6d :: Func
mz_array6d = userD "array6d"

mz_array_intersect :: Func
mz_array_intersect = userD "array_intersect"

mz_array_union :: Func
mz_array_union = userD "array_union"

mz_asin :: Func
mz_asin = userD "asin"

mz_asinh :: Func
mz_asinh = userD "asinh"

mz_assert :: Func
mz_assert = userD "assert"

mz_atan :: Func
mz_atan = userD "atan"

mz_atanh :: Func
mz_atanh = userD "atanh"

mz_bool2int :: Func
mz_bool2int = userD "bool2int"

mz_card :: Func
mz_card = userD "card"

mz_ceil :: Func
mz_ceil = userD "ceil"

mz_concat :: Func
mz_concat = userD "concat"

mz_cos :: Func
mz_cos = userD "cos"

mz_cosh :: Func
mz_cosh = userD "cosh"

mz_deopt :: Func
mz_deopt = userD "deopt"

mz_dom :: Func
mz_dom = userD "dom"

mz_dom_array :: Func
mz_dom_array = userD "dom_array"

mz_dom_size :: Func
mz_dom_size = userD "dom_size"

mz_exists :: Func
mz_exists = userD "exists"

mz_exp :: Func
mz_exp = userD "exp"

mz_fix :: Func
mz_fix = userD "fix"

mz_floor :: Func
mz_floor = userD "floor"

mz_forall :: Func
mz_forall = userD "forall"

mz_index_set :: Func
mz_index_set = userD "index_set"

mz_index_set_1of2 :: Func
mz_index_set_1of2 = userD "index_set_1of2"

mz_index_set_2of2 :: Func
mz_index_set_2of2 = userD "index_set_2of2"

mz_int2float :: Func
mz_int2float = userD "int2float"

mz_is_fixed :: Func
mz_is_fixed = userD "is_fixed"

mz_join :: Func
mz_join = userD "join"

mz_length :: Func
mz_length = userD "length"

mz_lb :: Func
mz_lb = userD "lb"

mz_lb_array :: Func
mz_lb_array = userD "lb_array"

mz_ln :: Func
mz_ln = userD "ln"

mz_log :: Func
mz_log = userD "log"

mz_log10 :: Func
mz_log10 = userD "log10"

mz_log2 :: Func
mz_log2 = userD "log2"

mz_max :: Func
mz_max = userD "max"

mz_min :: Func
mz_min = userD "min"

mz_occurs :: Func
mz_occurs = userD "occurs"

mz_pow :: Func
mz_pow = userD "pow"

mz_product :: Func
mz_product = userD "product"

mz_regular :: Func
mz_regular = userD "regular"

mz_round :: Func
mz_round = userD "round"

mz_set2array :: Func
mz_set2array = userD "set2array"

mz_show :: Func
mz_show = userD "show"

mz_show_float :: Func
mz_show_float = userD "show_float"

mz_show_int :: Func
mz_show_int = userD "show_int"

mz_sin :: Func
mz_sin = userD "sin"

mz_sinh :: Func
mz_sinh = userD "sinh"

mz_sqrt :: Func
mz_sqrt = userD "sqrt"

mz_sum :: Func
mz_sum = userD "sum"

mz_tan :: Func
mz_tan = userD "tan"

mz_tanh :: Func
mz_tanh = userD "tanh"

mz_trace :: Func
mz_trace = userD "trace"

mz_ub :: Func
mz_ub = userD "ub"

mz_ub_array :: Func
mz_ub_array = userD "ub_array"

mz_xorall :: Func
mz_xorall = userD "xorall"

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

type CompTail = ([Generator], Maybe Expr)

type Generator = ([Ident], Expr)
 
type TypeInst = (Inst, VarType)

type Param = (Inst, VarType, Ident)

type Ident = String

type Filename = String
