{-|
Module      : MZBuiltIns
Description : MiniZinc built-in predicates, tests and functions
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module uses definitions of Interfaces.MZAST to provide an easy interface to
MiniZinc built-in calls. It might be missing built-in calls that have been added
in recent releases of MiniZinc.
-}

module Interfaces.MZBuiltIns (
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
  mz_assert, mz_abort, mz_trace, mz_fix, mz_is_fixed
) where

import Interfaces.MZAST

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