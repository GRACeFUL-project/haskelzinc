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
  opPrec,
  -- * MiniZinc built-in operators
  -- ** Comparison operators
  mz_neq, mz_lt, mz_lte, mz_eq, mz_gt, mz_gte,
  -- ** Arithmetic operators
  mz_neg, mz_times, mz_plus, mz_minus, mz_div, mz_idiv, mz_mod,
  -- ** Logical operators
  mz_not, mz_rarrow, mz_and, mz_larrow, mz_lrarrow, mz_or, mz_xor,
  -- ** Set operators
  mz_range, mz_diff, mz_in, mz_intersect, mz_subset, mz_superset, mz_symdiff, mz_union,
  -- ** Array operators
  mz_pp,
  -- * MiniZinc built-in calls
  -- ** Arithmetic calls
  mz_abs, mz_arg_max, mz_arg_min, mz_max, mz_min, mz_pow, mz_product, mz_sqrt, mz_sum,
  -- ** Exponential and logarithmic calls
  mz_exp, mz_ln, mz_log, mz_log10, mz_log2,
  -- ** Trigonometric calls
  mz_acos, mz_acosh, mz_asin, mz_asinh, mz_atan, mz_atanh, mz_cos, mz_cosh, mz_sin, mz_sinh, mz_tan,
  mz_tanh,
  -- ** Logical calls
  mz_clause, mz_exists, mz_forall, mz_iffall, mz_xorall,
  -- ** Set calls
  mz_card, mz_array_intersect, mz_array_union,
  -- ** Array calls
  mz_array1d, mz_array2d, mz_array3d, mz_array4d, mz_array5d, mz_array6d, mz_arrayXd, 
  mz_col, mz_has_element, mz_has_index, mz_index_set, mz_index_set_1of2, mz_index_set_1of3,
  mz_index_set_1of4, mz_index_set_1of5, mz_index_set_1of6, mz_index_set_2of2, mz_index_set_2of3,
  mz_index_set_2of4, mz_index_set_2of5, mz_index_set_2of6, mz_index_set_3of3, mz_index_set_3of4,
  mz_index_set_3of5, mz_index_set_3of6, mz_index_set_4of4, mz_index_set_4of5, mz_index_set_4of6, 
  mz_index_set_5of5, mz_index_set_5of6, mz_index_set_6of6, mz_index_sets_agree, mz_length,
  mz_reverse, mz_row,
  -- ** Array sorting calls
  mz_arg_sort, mz_sort, mz_sort_by,
  -- ** Coercion calls
  mz_bool2float, mz_bool2int, mz_ceil, mz_floor, mz_int2float, mz_round, mz_set2array,
  -- ** String calls
  mz_concat, mz_file_path, mz_format, mz_join, mz_show, mz_show2d, mz_show3d, mz_showJSON, 
  mz_show_float, mz_show_int, mz_strig_length,
  -- ** Reflection calls
  mz_dom, mz_dom_array, mz_dom_bounds_array, mz_dom_size, mz_fix, mz_has_bounds, mz_has_ub_set,
  mz_is_fixed, mz_lb, mz_lb_array, mz_ub, mz_ub_array,
  -- ** Assertions and debugging calls
  mz_abort, mz_assert, mz_trace, mz_trace_stdout,
  -- ** Calls for @Enum@s
  mz_enum_next, mz_enum_prev, mz_to_enum,
  -- ** Calls for @Optional@s
  mz_absent, mz_deopt, mz_occurs,
  -- ** Random number generator calls
  mz_bernoulli, mz_binomial, mz_cauchy, mz_chisquared, mz_discrete_distribution, mz_exponential, 
  mz_fdistribution, mz_gamma, mz_lognormal, mz_normal, mz_poisson, mz_tdistribution, mz_uniform, 
  mz_weibull,
  -- ** Special constraints
  mz_implied_constraint, mz_redundant_constraint, mz_symmetry_breaking_constraint,
  -- ** Language information
  mz_mzn_compiler_version, mz_mzn_version_to_string
) where

import Interfaces.MZAST

-- MiniZinc calls

-- Arithmetic calls
mz_abs     = CName "abs"
mz_arg_max = CName "arg_max"
mz_arg_min = CName "arg_min"
mz_max     = CName "max"
mz_min     = CName "min"
mz_pow     = CName "pow"
mz_product = CName "product"
mz_sqrt    = CName "sqrt"
mz_sum     = CName "sum"

-- Exponential and logarithmic calls
mz_exp   = CName "exp"
mz_ln    = CName "ln"
mz_log   = CName "log"
mz_log10 = CName "log10"
mz_log2  = CName "log2"

-- Trigonometric calls
mz_acos  = CName "acos"
mz_acosh = CName "acosh"
mz_asin  = CName "asin"
mz_asinh = CName "asinh"
mz_atan  = CName "atan"
mz_atanh = CName "atanh"
mz_cos   = CName "cos"
mz_cosh  = CName "cosh"
mz_sin   = CName "sin"
mz_sinh  = CName "sinh"
mz_tan   = CName "tan"
mz_tanh  = CName "tanh"

-- Logical calls
mz_clause = CName "clause"
mz_exists = CName "exists"
mz_forall = CName "forall"
mz_iffall = CName "iffall"
mz_xorall = CName "xorall"

-- Set calls
mz_array_intersect = CName "array_intersect"
mz_array_union     = CName "array_union"
mz_card            = CName "card"

-- Array calls
mz_array1d     = CName "array1d"
mz_array2d     = CName "array2d"
mz_array3d     = CName "array3d"
mz_array4d     = CName "array4d"
mz_array5d     = CName "array5d"
mz_array6d     = CName "array6d"
mz_arrayXd     = CName "arrayXd"
mz_col         = CName "col"
mz_has_element = CName "has_element"
mz_has_index   = CName "has_index"
mz_index_set   = CName "index_set"
mz_index_set_1of2 = CName "index_set_1of2"
mz_index_set_1of3 = CName "index_set_1of3"
mz_index_set_1of4 = CName "index_set_1of4"
mz_index_set_1of5 = CName "index_set_1of5"
mz_index_set_1of6 = CName "index_set_1of6"
mz_index_set_2of2 = CName "index_set_2of2"
mz_index_set_2of3 = CName "index_set_2of3"
mz_index_set_2of4 = CName "index_set_2of4"
mz_index_set_2of5 = CName "index_set_2of5"
mz_index_set_2of6 = CName "index_set_2of6"
mz_index_set_3of3 = CName "index_set_3of3"
mz_index_set_3of4 = CName "index_set_3of4"
mz_index_set_3of5 = CName "index_set_3of5"
mz_index_set_3of6 = CName "index_set_3of6"
mz_index_set_4of4 = CName "index_set_4of4"
mz_index_set_4of5 = CName "index_set_4of5"
mz_index_set_4of6 = CName "index_set_4of6"
mz_index_set_5of5 = CName "index_set_5of5"
mz_index_set_5of6 = CName "index_set_5of6"
mz_index_set_6of6 = CName "index_set_6of6"
mz_index_sets_agree = CName "index_sets_agree"
mz_length  = CName "length"
mz_reverse = CName "reverse"
mz_row     = CName "row"

-- Array sorting calls
mz_arg_sort = CName "arg_sort"
mz_sort     = CName "sort"
mz_sort_by  = CName "sort_by"

-- Coercion calls
mz_bool2float = CName "bool2float"
mz_bool2int = CName "bool2int"
mz_ceil = CName "ceil"
mz_floor = CName "floor"
mz_int2float = CName "int2float"
mz_round = CName "round"
mz_set2array = CName "set2array"

-- String calls
mz_concat    = CName "concat"
mz_file_path = CName "file_path"
mz_format    = CName "format"
mz_join      = CName "join"
mz_show      = CName "show"
mz_show2d    = CName "show2d"
mz_show3d    = CName "show3d"
mz_showJSON  = CName "showJSON"
mz_show_float = CName "show_float"
mz_show_int = CName "show_int"
mz_strig_length = CName "string_length"

-- Reflection calls
mz_dom              = CName "dom"
mz_dom_array        = CName "dom_array"
mz_dom_bounds_array = CName "dom_bounds_array"
mz_dom_size         = CName "dom_size"
mz_fix              = CName "fix"
mz_has_bounds       = CName "has_bounds"
mz_has_ub_set       = CName "has_ub_set"
mz_is_fixed         = CName "is_fixed"
mz_lb               = CName "lb"
mz_lb_array         = CName "lb_array"
mz_ub               = CName "ub"
mz_ub_array         = CName "ub_array"

-- Assertions and debugging calls
mz_abort        = CName "abort"
mz_assert       = CName "assert"
mz_trace        = CName "trace"
mz_trace_stdout = CName "trace_stdout"

-- Calls for @Enum@s
mz_enum_next = CName "enum_next"
mz_enum_prev = CName "enum_prev"
mz_to_enum   = CName "to_enum"

-- Calls for Optionals

-- Random number generator calls
mz_bernoulli      = CName "bernoulli"
mz_binomial       = CName "binomial"
mz_cauchy         = CName "cauchy"
mz_chisquared     = CName "chisquared"
mz_discrete_distribution = CName "discrete_distribution"
mz_exponential    = CName "exponential"
mz_fdistribution  = CName "fdistribution"
mz_gamma          = CName "gamma"
mz_lognormal      = CName "lognormal"
mz_normal         = CName "normal"
mz_poisson        = CName "poisson"
mz_tdistribution  = CName "tdistribution"
mz_uniform        = CName "uniform"
mz_weibull        = CName "weibull"

-- Special constraints
mz_implied_constraint           = CName "implied_constraint"
mz_redundant_constraint         = CName "redundant_constraint"
mz_symmetry_breaking_constraint = CName "symmetry_breaking_constraint"

-- Language information
mz_mzn_compiler_version = CName "mzn_compiler_version"
mz_mzn_version_to_string = CName "mzn_version_to_string"

-- MiniZinc unary operators
mz_neg = Uop "-"
mz_not = Uop "not"

-- MiniZinc binary operators
mz_absent = CName "absent"
mz_deopt  = CName "deopt"
mz_occurs = CName "occurs"
--mz_regular = CName "regular"

-- Comparison operators
mz_neq = Bop "!="
mz_lt  = Bop "<"
mz_lte = Bop "<="
mz_eq  = Bop "="
mz_gt  = Bop ">"
mz_gte = Bop ">="

-- Arithmetic operators
mz_times = Bop "*"
mz_plus  = Bop "+"
mz_minus = Bop "-"
mz_div   = Bop "/"
mz_idiv  = Bop "div"
mz_mod   = Bop "mod"

-- Logical operators
mz_rarrow  = Bop "->"
mz_and     = Bop "/\\"
mz_larrow  = Bop "<-"
mz_lrarrow = Bop "<->"
mz_or      = Bop "\\/"
mz_xor     = Bop "xor"

-- Set operators
mz_range     = Bop ".."
mz_diff      = Bop "diff"
mz_in        = Bop "in"
mz_intersect = Bop "intersect"
mz_subset    = Bop "subset"
mz_superset  = Bop "superset"
mz_symdiff   = Bop "symdiff"
mz_union     = Bop "union"

-- Array operators
mz_pp = Bop "++"

opPrec :: Bop -> Int
opPrec bop
  | bop == mz_lrarrow = 7
  | bop == mz_rarrow  = 7
  | bop == mz_larrow  = 7
  | bop == mz_and     = 7
  | bop == mz_or      = 7
  | bop == mz_eq      = 8
  | bop == mz_neq     = 8
  | bop == mz_times   = 9
  | bop == mz_mod     = 9
  | otherwise         = 10
{-
prec mz_lrarrow  = 7 
prec mz_rarrow   = 7
prec mz_larrow   = 7
prec mz_and      = 7 
prec mz_or       = 7
prec mz_eq      = 8
prec mz_neq      = 8
prec mz_times    = 9
prec mz_mod      = 9
prec _        = 10
-}
-- MiniZinc annotations
{-
data AnnName
  = AName Ident
  -- General annotations
  | MZadd_to_output
  | MZis_defined_var
  | MZis_reverse-map
  | MZmaybe_partial
  | MZoutput_var
  | MZpromise_total
  | MZvar_is_introduced
  | MZdefines_var
  | MZdoc_comment
  | MZoutput_array
  -- Propagation strength annotations
  | MZbounds
  | MZdomain
  -- Search annotations
  | MZbool_search
  | MZ_float_search
  | MZint_search
  |
-}