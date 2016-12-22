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
  mz_times, mz_plus, mz_minus, mz_div, mz_idiv, mz_mod,
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
  mz_mzn_compiler_version, mz_mzn_version_to_string,
  -- * MiniZinc annotations
  -- ** General annotations
  mz_add_to_output, mz_is_defined_var, mz_is_reverse_map, mz_maybe_partial, mz_output_var,
  mz_promise_total, mz_var_is_introduced, mz_defines_var, mz_doc_comment, mz_output_array,
  -- ** Propagation strength annotations
  mz_bounds, mz_domain,
  -- ** Search annotations
  mz_bool_search, mz_float_search, mz_int_search, mz_seq_search, mz_set_search,
  -- *** Variable selection annotations
  mz_anti_first_fail, mz_dom_w_deg, mz_first_fail, mz_impact, mz_input_order, mz_largest
  mz_max_regret, mz_most_constrained, mz_occurrence, mz_smallest,
  -- *** Value choice annotations
  mz_indomain, mz_indomain_interval, mz_indomain_max, mz_indomain_median, mz_indomain_middle,
  mz_indomain_min, mz_indomain_random, mz_indomain_reverse_split, mz_indomain_split,
  mz_indomain_split_random, mz_outdomain_max, mz_outdomain_median, mz_outdomain_min,
  mz_outdomain_random,
  -- *** Exploration strategy annotations
) where

import Interfaces.MZAST

-- MiniZinc calls

-- Arithmetic calls
mz_abs     = Callable "abs"
mz_arg_max = Callable "arg_max"
mz_arg_min = Callable "arg_min"
mz_max     = Callable "max"
mz_min     = Callable "min"
mz_pow     = Callable "pow"
mz_product = Callable "product"
mz_sqrt    = Callable "sqrt"
mz_sum     = Callable "sum"

-- Exponential and logarithmic calls
mz_exp   = Callable "exp"
mz_ln    = Callable "ln"
mz_log   = Callable "log"
mz_log10 = Callable "log10"
mz_log2  = Callable "log2"

-- Trigonometric calls
mz_acos  = Callable "acos"
mz_acosh = Callable "acosh"
mz_asin  = Callable "asin"
mz_asinh = Callable "asinh"
mz_atan  = Callable "atan"
mz_atanh = Callable "atanh"
mz_cos   = Callable "cos"
mz_cosh  = Callable "cosh"
mz_sin   = Callable "sin"
mz_sinh  = Callable "sinh"
mz_tan   = Callable "tan"
mz_tanh  = Callable "tanh"

-- Logical calls
mz_clause = Callable "clause"
mz_exists = Callable "exists"
mz_forall = Callable "forall"
mz_iffall = Callable "iffall"
mz_xorall = Callable "xorall"

-- Set calls
mz_array_intersect = Callable "array_intersect"
mz_array_union     = Callable "array_union"
mz_card            = Callable "card"

-- Array calls
mz_array1d     = Callable "array1d"
mz_array2d     = Callable "array2d"
mz_array3d     = Callable "array3d"
mz_array4d     = Callable "array4d"
mz_array5d     = Callable "array5d"
mz_array6d     = Callable "array6d"
mz_arrayXd     = Callable "arrayXd"
mz_col         = Callable "col"
mz_has_element = Callable "has_element"
mz_has_index   = Callable "has_index"
mz_index_set   = Callable "index_set"
mz_index_set_1of2 = Callable "index_set_1of2"
mz_index_set_1of3 = Callable "index_set_1of3"
mz_index_set_1of4 = Callable "index_set_1of4"
mz_index_set_1of5 = Callable "index_set_1of5"
mz_index_set_1of6 = Callable "index_set_1of6"
mz_index_set_2of2 = Callable "index_set_2of2"
mz_index_set_2of3 = Callable "index_set_2of3"
mz_index_set_2of4 = Callable "index_set_2of4"
mz_index_set_2of5 = Callable "index_set_2of5"
mz_index_set_2of6 = Callable "index_set_2of6"
mz_index_set_3of3 = Callable "index_set_3of3"
mz_index_set_3of4 = Callable "index_set_3of4"
mz_index_set_3of5 = Callable "index_set_3of5"
mz_index_set_3of6 = Callable "index_set_3of6"
mz_index_set_4of4 = Callable "index_set_4of4"
mz_index_set_4of5 = Callable "index_set_4of5"
mz_index_set_4of6 = Callable "index_set_4of6"
mz_index_set_5of5 = Callable "index_set_5of5"
mz_index_set_5of6 = Callable "index_set_5of6"
mz_index_set_6of6 = Callable "index_set_6of6"
mz_index_sets_agree = Callable "index_sets_agree"
mz_length  = Callable "length"
mz_reverse = Callable "reverse"
mz_row     = Callable "row"

-- Array sorting calls
mz_arg_sort = Callable "arg_sort"
mz_sort     = Callable "sort"
mz_sort_by  = Callable "sort_by"

-- Coercion calls
mz_bool2float = Callable "bool2float"
mz_bool2int = Callable "bool2int"
mz_ceil = Callable "ceil"
mz_floor = Callable "floor"
mz_int2float = Callable "int2float"
mz_round = Callable "round"
mz_set2array = Callable "set2array"

-- String calls
mz_concat    = Callable "concat"
mz_file_path = Callable "file_path"
mz_format    = Callable "format"
mz_join      = Callable "join"
mz_show      = Callable "show"
mz_show2d    = Callable "show2d"
mz_show3d    = Callable "show3d"
mz_showJSON  = Callable "showJSON"
mz_show_float = Callable "show_float"
mz_show_int = Callable "show_int"
mz_strig_length = Callable "string_length"

-- Reflection calls
mz_dom              = Callable "dom"
mz_dom_array        = Callable "dom_array"
mz_dom_bounds_array = Callable "dom_bounds_array"
mz_dom_size         = Callable "dom_size"
mz_fix              = Callable "fix"
mz_has_bounds       = Callable "has_bounds"
mz_has_ub_set       = Callable "has_ub_set"
mz_is_fixed         = Callable "is_fixed"
mz_lb               = Callable "lb"
mz_lb_array         = Callable "lb_array"
mz_ub               = Callable "ub"
mz_ub_array         = Callable "ub_array"

-- Assertions and debugging calls
mz_abort        = Callable "abort"
mz_assert       = Callable "assert"
mz_trace        = Callable "trace"
mz_trace_stdout = Callable "trace_stdout"

-- Calls for @Enum@s
mz_enum_next = Callable "enum_next"
mz_enum_prev = Callable "enum_prev"
mz_to_enum   = Callable "to_enum"

-- Calls for Optionals

-- Random number generator calls
mz_bernoulli      = Callable "bernoulli"
mz_binomial       = Callable "binomial"
mz_cauchy         = Callable "cauchy"
mz_chisquared     = Callable "chisquared"
mz_discrete_distribution = Callable "discrete_distribution"
mz_exponential    = Callable "exponential"
mz_fdistribution  = Callable "fdistribution"
mz_gamma          = Callable "gamma"
mz_lognormal      = Callable "lognormal"
mz_normal         = Callable "normal"
mz_poisson        = Callable "poisson"
mz_tdistribution  = Callable "tdistribution"
mz_uniform        = Callable "uniform"
mz_weibull        = Callable "weibull"

-- Special constraints
mz_implied_constraint           = Callable "implied_constraint"
mz_redundant_constraint         = Callable "redundant_constraint"
mz_symmetry_breaking_constraint = Callable "symmetry_breaking_constraint"

-- Language information
mz_mzn_compiler_version = Callable "mzn_compiler_version"
mz_mzn_version_to_string = Callable "mzn_version_to_string"

-- MiniZinc unary operators

-- MiniZinc binary operators
mz_absent = Callable "absent"
mz_deopt  = Callable "deopt"
mz_occurs = Callable "occurs"
--mz_regular = Callable "regular"

-- Comparison operators
mz_neq = Op "!="
mz_lt  = Op "<"
mz_lte = Op "<="
mz_eq  = Op "="
mz_gt  = Op ">"
mz_gte = Op ">="

-- Arithmetic operators
mz_times = Op "*"
mz_plus  = Op "+"
mz_minus = Op "-"
mz_div   = Op "/"
mz_idiv  = Op "div"
mz_mod   = Op "mod"

-- Logical operators
mz_rarrow  = Op "->"
mz_and     = Op "/\\"
mz_larrow  = Op "<-"
mz_lrarrow = Op "<->"
mz_not     = Op "not"
mz_or      = Op "\\/"
mz_xor     = Op "xor"

-- Set operators
mz_range     = Op ".."
mz_diff      = Op "diff"
mz_in        = Op "in"
mz_intersect = Op "intersect"
mz_subset    = Op "subset"
mz_superset  = Op "superset"
mz_symdiff   = Op "symdiff"
mz_union     = Op "union"

-- Array operators
mz_pp = Op "++"

-- | Returns the precedence of certain defined operators. This function is used for reducing
-- the parentheses when printing an expression.
opPrec :: Op -> Int
opPrec op
  | op == mz_lrarrow = 7
  | op == mz_rarrow  = 7
  | op == mz_larrow  = 7
  | op == mz_and     = 7
  | op == mz_or      = 7
  | op == mz_eq      = 8
  | op == mz_neq     = 8
  | op == mz_times   = 9
  | op == mz_mod     = 9
  | otherwise        = 10

-- MiniZinc annotations

-- General annotations
mz_add_to_output     = Annotation "add_to_output"
mz_is_defined_var    = Annotation "is_defined_var"
mz_is_reverse_map    = Annotation "is_reverse_map"
mz_maybe_partial     = Annotation "maybe_partial"
mz_output_var        = Annotation "output_var"
mz_promise_total     = Annotation "promise_total"
mz_var_is_introduced = Annotation "var_is_introduced"
mz_defines_var       = Annotation "defines_var"
mz_doc_comment       = Annotation "doc_comment"
mz_output_array      = Annotation "output_array"

-- Propagation strength annotations
mz_bounds = Annotation "bounds"
mz_domain = Annotation "domain"

-- Search annotations
mz_bool_search  = Annotation "bool_search"
mz_float_search = Annotation "float_search"
mz_int_search   = Annotation "int_search"
mz_seq_search   = Annotation "seq_search"
mz_set_search   = Annotation "set_search"

-- Variable selection annotations
mz_anti_first_fail  = Annotation "anti_first_fail"
mz_dom_w_deg        = Annotation "dom_w_deg"
mz_first_fail       = Annotation "first_fail"
mz_impact           = Annotation "impact"
mz_input_order      = Annotation "input_order"
mz_largest          = Annotation "largest"
mz_max_regret       = Annotation "max_regret"
mz_most_constrained = Annotation "most_constrained"
mz_occurrence       = Annotation "occurrence"
mz_smallest         = Annotation "smallest"

-- Value choice annotations
mz_indomain               = Annotation "indomain"
mz_indomain_interval      = Annotation "indomain_interval"
mz_indomain_max           = Annotation "indomain_max"
mz_indomain_median        = Annotation "indomain_median"
mz_indomain_middle        = Annotation "indomain_middle"
mz_indomain_min           = Annotation "indomain_min"
mz_indomain_random        = Annotation "indomain_random"
mz_indomain_reverse_split = Annotation "indomain_reverse_split"
mz_indomain_split         = Annotation "indomain_split"
mz_indomain_split_random  = Annotation "indomain_split_random"
mz_outdomain_max          = Annotation "outdomain_max"
mz_outdomain_median       = Annotation "outdomain_median"
mz_outdomain_min          = Annotation "outdomain_min"
mz_outdomain_random       = Annotation "outdomain_random"

-- Exploration strategy annotations
mz_complete = Annotation "complete"