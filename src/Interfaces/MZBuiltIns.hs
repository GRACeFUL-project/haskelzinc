{-|
Module      : MZBuiltIns
Description : MiniZinc built-in predicates, tests and functions
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module uses definitions of "Interfaces.MZAST" to provide an easy interface to
MiniZinc built-in calls. It might be missing built-in calls that have been added
in recent releases of MiniZinc.
-}

module Interfaces.MZBuiltIns (
  opPrec,
  -- * MiniZinc built-in operators
  -- ** Comparison operators
  (!=.), (>.), (>=.), (=.=), (<.), (<=.),
  -- ** Arithmetic operators
  (*.), (+.), plus_, (-.), minus_, (/.), _div_, _mod_,
  -- ** Logical operators
  not_, (->.), (/\.), (<-.), (<->.), (\/.), _xor_,
  -- ** Set operators
  (...), _diff_, _in_, _intersect_, _subset_, _superset_, _symdiff_, _union_,
  -- ** Array operators
  (++.),
  -- * MiniZinc built-in calls
  -- | All functions, predicates, tests and annotations are named after their MiniZinc 
  -- name prefixed by @mz_@. Operators' representation follows a different convention.
  
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
  -- * MiniZinc global constraints
  mz_all_different,
  -- * MiniZinc annotations
  -- ** General annotations
  mz_add_to_output, mz_is_defined_var, mz_is_reverse_map, mz_maybe_partial, mz_output_var,
  mz_promise_total, mz_var_is_introduced, mz_defines_var, mz_doc_comment, mz_output_array,
  -- ** Propagation strength annotations
  mz_bounds, mz_domain,
  -- ** Search annotations
  mz_bool_search, mz_float_search, mz_int_search, mz_seq_search, mz_set_search,
  -- *** Variable selection annotations
  mz_anti_first_fail, mz_dom_w_deg, mz_first_fail, mz_impact, mz_input_order, mz_largest,
  mz_max_regret, mz_most_constrained, mz_occurrence, mz_smallest,
  -- *** Value choice annotations
  mz_indomain, mz_indomain_interval, mz_indomain_max, mz_indomain_median, mz_indomain_middle,
  mz_indomain_min, mz_indomain_random, mz_indomain_reverse_split, mz_indomain_split,
  mz_indomain_split_random, mz_outdomain_max, mz_outdomain_median, mz_outdomain_min,
  mz_outdomain_random,
  -- *** Exploration strategy annotations
  mz_complete
) where

import Interfaces.MZAST (Expr(Bi, U), Op, infOp, prefCall, Annotation(Annotation))


-- MiniZinc calls

-- Arithmetic calls
mz_abs     = prefCall "abs"
mz_arg_max = prefCall "arg_max"
mz_arg_min = prefCall "arg_min"
mz_max     = prefCall "max"
mz_min     = prefCall "min"
mz_pow     = prefCall "pow"
mz_product = prefCall "product"
mz_sqrt    = prefCall "sqrt"
mz_sum     = prefCall "sum"

-- Exponential and logarithmic calls
mz_exp   = prefCall "exp"
mz_ln    = prefCall "ln"
mz_log   = prefCall "log"
mz_log10 = prefCall "log10"
mz_log2  = prefCall "log2"

-- Trigonometric calls
mz_acos  = prefCall "acos"
mz_acosh = prefCall "acosh"
mz_asin  = prefCall "asin"
mz_asinh = prefCall "asinh"
mz_atan  = prefCall "atan"
mz_atanh = prefCall "atanh"
mz_cos   = prefCall "cos"
mz_cosh  = prefCall "cosh"
mz_sin   = prefCall "sin"
mz_sinh  = prefCall "sinh"
mz_tan   = prefCall "tan"
mz_tanh  = prefCall "tanh"

-- Logical calls
mz_clause = prefCall "clause"
mz_exists = prefCall "exists"
mz_forall = prefCall "forall"
mz_iffall = prefCall "iffall"
mz_xorall = prefCall "xorall"

-- Set calls
mz_array_intersect = prefCall "array_intersect"
mz_array_union     = prefCall "array_union"
mz_card            = prefCall "card"

-- Array calls
mz_array1d     = prefCall "array1d"
mz_array2d     = prefCall "array2d"
mz_array3d     = prefCall "array3d"
mz_array4d     = prefCall "array4d"
mz_array5d     = prefCall "array5d"
mz_array6d     = prefCall "array6d"
mz_arrayXd     = prefCall "arrayXd"
mz_col         = prefCall "col"
mz_has_element = prefCall "has_element"
mz_has_index   = prefCall "has_index"
mz_index_set   = prefCall "index_set"
mz_index_set_1of2 = prefCall "index_set_1of2"
mz_index_set_1of3 = prefCall "index_set_1of3"
mz_index_set_1of4 = prefCall "index_set_1of4"
mz_index_set_1of5 = prefCall "index_set_1of5"
mz_index_set_1of6 = prefCall "index_set_1of6"
mz_index_set_2of2 = prefCall "index_set_2of2"
mz_index_set_2of3 = prefCall "index_set_2of3"
mz_index_set_2of4 = prefCall "index_set_2of4"
mz_index_set_2of5 = prefCall "index_set_2of5"
mz_index_set_2of6 = prefCall "index_set_2of6"
mz_index_set_3of3 = prefCall "index_set_3of3"
mz_index_set_3of4 = prefCall "index_set_3of4"
mz_index_set_3of5 = prefCall "index_set_3of5"
mz_index_set_3of6 = prefCall "index_set_3of6"
mz_index_set_4of4 = prefCall "index_set_4of4"
mz_index_set_4of5 = prefCall "index_set_4of5"
mz_index_set_4of6 = prefCall "index_set_4of6"
mz_index_set_5of5 = prefCall "index_set_5of5"
mz_index_set_5of6 = prefCall "index_set_5of6"
mz_index_set_6of6 = prefCall "index_set_6of6"
mz_index_sets_agree = prefCall "index_sets_agree"
mz_length  = prefCall "length"
mz_reverse = prefCall "reverse"
mz_row     = prefCall "row"

-- Array sorting calls
mz_arg_sort = prefCall "arg_sort"
mz_sort     = prefCall "sort"
mz_sort_by  = prefCall "sort_by"

-- Coercion calls
mz_bool2float = prefCall "bool2float"
mz_bool2int   = prefCall "bool2int"
mz_ceil       = prefCall "ceil"
mz_floor      = prefCall "floor"
mz_int2float  = prefCall "int2float"
mz_round      = prefCall "round"
mz_set2array  = prefCall "set2array"

-- String calls
mz_concat       = prefCall "concat"
mz_file_path    = prefCall "file_path"
mz_format       = prefCall "format"
mz_join         = prefCall "join"
mz_show         = prefCall "show"
mz_show2d       = prefCall "show2d"
mz_show3d       = prefCall "show3d"
mz_showJSON     = prefCall "showJSON"
mz_show_float   = prefCall "show_float"
mz_show_int     = prefCall "show_int"
mz_strig_length = prefCall "string_length"

-- Reflection calls
mz_dom              = prefCall "dom"
mz_dom_array        = prefCall "dom_array"
mz_dom_bounds_array = prefCall "dom_bounds_array"
mz_dom_size         = prefCall "dom_size"
mz_fix              = prefCall "fix"
mz_has_bounds       = prefCall "has_bounds"
mz_has_ub_set       = prefCall "has_ub_set"
mz_is_fixed         = prefCall "is_fixed"
mz_lb               = prefCall "lb"
mz_lb_array         = prefCall "lb_array"
mz_ub               = prefCall "ub"
mz_ub_array         = prefCall "ub_array"

-- Assertions and debugging calls
mz_abort        = prefCall "abort"
mz_assert       = prefCall "assert"
mz_trace        = prefCall "trace"
mz_trace_stdout = prefCall "trace_stdout"

-- Calls for @Enum@s
mz_enum_next = prefCall "enum_next"
mz_enum_prev = prefCall "enum_prev"
mz_to_enum   = prefCall "to_enum"

-- Calls for Optionals

-- Random number generator calls
mz_bernoulli      = prefCall "bernoulli"
mz_binomial       = prefCall "binomial"
mz_cauchy         = prefCall "cauchy"
mz_chisquared     = prefCall "chisquared"
mz_discrete_distribution = prefCall "discrete_distribution"
mz_exponential    = prefCall "exponential"
mz_fdistribution  = prefCall "fdistribution"
mz_gamma          = prefCall "gamma"
mz_lognormal      = prefCall "lognormal"
mz_normal         = prefCall "normal"
mz_poisson        = prefCall "poisson"
mz_tdistribution  = prefCall "tdistribution"
mz_uniform        = prefCall "uniform"
mz_weibull        = prefCall "weibull"

-- Special constraints
mz_implied_constraint           = prefCall "implied_constraint"
mz_redundant_constraint         = prefCall "redundant_constraint"
mz_symmetry_breaking_constraint = prefCall "symmetry_breaking_constraint"

-- Language information
mz_mzn_compiler_version  = prefCall "mzn_compiler_version"
mz_mzn_version_to_string = prefCall "mzn_version_to_string"

-- MiniZinc operators

infixl 3 /\., <->., <-., ->., \/., `_xor_`
infix 4 <., <=., >., >=., =.=, !=.
infix 5 `_in_`, `_subset_`, `_superset_`
infixl 6 `_union_`, `_diff_`, `_symdiff_`, ...
infixl 7 +., -.
infixl 8 *., /., `_div_`, `_mod_`, `_intersect_`, ++.

mz_absent = prefCall "absent"
mz_deopt  = prefCall "deopt"
mz_occurs = prefCall "occurs"
mz_regular = prefCall "regular"

-- Comparison operators
mz_neq = infOp "!="
mz_lt  = infOp "<" 
mz_lte = infOp "<="
mz_eq  = infOp "=" 
mz_gt  = infOp ">" 
mz_gte = infOp ">="
-- | @!=@
(!=.) = Bi mz_neq
-- | @<@
(<.)  = Bi mz_lt
-- | @<=@
(<=.) = Bi mz_lte
-- | @=@
(=.=) = Bi mz_eq
-- | @>@
(>.)  = Bi mz_gt
-- | @>=@
(>=.) = Bi mz_gte

-- Arithmetic operators
mz_times = infOp "*"  
mz_plus  = infOp "+"  
mz_minus = infOp "-"  
mz_div   = infOp "/"  
mz_idiv  = infOp "div"
mz_mod   = infOp "mod"
-- | @*@
(*.)   = Bi mz_times
-- | @+@ (the binary operator)
(+.)   = Bi mz_plus
-- | @+@ (the unary operator)
plus_  = U  mz_plus
-- | @-@ (the binary operator)
(-.)   = Bi mz_minus
-- | @-@ (the unary operator)
minus_ = U  mz_minus
-- | @/@
(/.)   = Bi mz_div
-- | @div@
_div_  = Bi mz_idiv
-- | @mod@
_mod_  = Bi mz_mod

-- Logical operators
mz_rarrow  = infOp "->"
mz_and     = infOp "/\\"
mz_larrow  = infOp "<-"
mz_lrarrow = infOp "<->"
mz_not     = infOp "not" 
mz_or      = infOp "\\/" 
mz_xor     = infOp "xor"
-- | @->@
(->.)  = Bi mz_rarrow
-- | @\/\\@
(/\.)  = Bi mz_and 
-- | @<-@
(<-.)  = Bi mz_larrow
-- | @<->@
(<->.) = Bi mz_lrarrow
-- | @not@
not_   = U  mz_not
-- | @\\\/@
(\/.)  = Bi mz_or
-- | @xor@
_xor_  = Bi mz_xor

-- Set operators
mz_range     = infOp ".."
mz_diff      = infOp "diff"
mz_in        = infOp "in"
mz_intersect = infOp "intersect"
mz_subset    = infOp "subset"
mz_superset  = infOp "superset"
mz_symdiff   = infOp "symdiff"
mz_union     = infOp "union"
-- | @..@
(...)        = Bi mz_range
-- | @diff@
_diff_       = Bi mz_diff
-- | @in@
_in_         = Bi mz_in
-- | @intersect@
_intersect_  = Bi mz_intersect
-- | @subset@
_subset_     = Bi mz_subset
-- | @superset@
_superset_   = Bi mz_superset
-- | @symdiff@
_symdiff_    = Bi mz_symdiff
-- | @union@
_union_      = Bi mz_union

-- Array operators
mz_pp = infOp "++"
-- | @++@
(++.) = Bi mz_pp

-- | Returns the precedence of certain defined operators. This function is used for 
-- reducing the parentheses when printing an expression.
opPrec :: Op -> Int
opPrec op
  | op == mz_pp        = 1
  | op == mz_times     = 2
  | op == mz_div       = 2
  | op == mz_mod       = 2
  | op == mz_idiv      = 2
  | op == mz_intersect = 2
  | op == mz_plus      = 3
  | op == mz_minus     = 3
  | op == mz_range     = 4
  | op == mz_union     = 5
  | op == mz_diff      = 5
  | op == mz_symdiff   = 5
  | op == mz_in        = 6
  | op == mz_subset    = 6
  | op == mz_superset  = 6
  | op == mz_neq       = 7
  | op == mz_lt        = 7
  | op == mz_lte       = 7
  | op == mz_eq        = 7
  | op == mz_gt        = 7
  | op == mz_gte       = 7
  | op == mz_and       = 8
  | op == mz_or        = 9
  | op == mz_xor       = 9
  | op == mz_rarrow    = 10
  | op == mz_larrow    = 10
  | op == mz_lrarrow   = 11
  | otherwise          = 15

  
-- MiniZinf global constraints
mz_all_different = prefCall "all_different"

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
