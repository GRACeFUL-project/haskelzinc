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
  (--.), _diff_, _in_, _intersect_, _subset_, _superset_, _symdiff_, _union_,
  -- ** Array operators
  (++.),
  -- * MiniZinc built-in calls
  -- | All functions, predicates, tests and annotations are named after their MiniZinc 
  -- name prefixed by 'mz_'. Operators' representation follows a different convention.
  
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
  mz_anti_first_fail, mz_dom_w_deg, mz_first_fail, mz_impact, mz_input_order, mz_largest,
  mz_max_regret, mz_most_constrained, mz_occurrence, mz_smallest,
  -- *** Value choice annotations
  mz_indomain, mz_indomain_interval, mz_indomain_max, mz_indomain_median, mz_indomain_middle,
  mz_indomain_min, mz_indomain_random, mz_indomain_reverse_split, mz_indomain_split,
  mz_indomain_split_random, mz_outdomain_max, mz_outdomain_median, mz_outdomain_min,
  mz_outdomain_random,
  -- *** Exploration strategy annotations
  mz_complete,
  -- Others
  call
) where

import Interfaces.MZASTBase

call :: Ident -> [Expr] -> Expr
call name = Call name . map toSimpleExpr

-- MiniZinc calls

-- Arithmetic calls
mz_abs     = call "abs"
mz_arg_max = call "arg_max"
mz_arg_min = call "arg_min"
mz_max     = call "max"
mz_min     = call "min"
mz_pow     = call "pow"
mz_product = call "product"
mz_sqrt    = call "sqrt"
mz_sum     = call "sum"

-- Exponential and logarithmic calls
mz_exp   = call "exp"
mz_ln    = call "ln"
mz_log   = call "log"
mz_log10 = call "log10"
mz_log2  = call "log2"

-- Trigonometric calls
mz_acos  = call "acos"
mz_acosh = call "acosh"
mz_asin  = call "asin"
mz_asinh = call "asinh"
mz_atan  = call "atan"
mz_atanh = call "atanh"
mz_cos   = call "cos"
mz_cosh  = call "cosh"
mz_sin   = call "sin"
mz_sinh  = call "sinh"
mz_tan   = call "tan"
mz_tanh  = call "tanh"

-- Logical calls
mz_clause = call "clause"
mz_exists = call "exists"
mz_forall = call "forall"
mz_iffall = call "iffall"
mz_xorall = call "xorall"

-- Set calls
mz_array_intersect = call "array_intersect"
mz_array_union     = call "array_union"
mz_card            = call "card"

-- Array calls
mz_array1d     = call "array1d"
mz_array2d     = call "array2d"
mz_array3d     = call "array3d"
mz_array4d     = call "array4d"
mz_array5d     = call "array5d"
mz_array6d     = call "array6d"
mz_arrayXd     = call "arrayXd"
mz_col         = call "col"
mz_has_element = call "has_element"
mz_has_index   = call "has_index"
mz_index_set   = call "index_set"
mz_index_set_1of2 = call "index_set_1of2"
mz_index_set_1of3 = call "index_set_1of3"
mz_index_set_1of4 = call "index_set_1of4"
mz_index_set_1of5 = call "index_set_1of5"
mz_index_set_1of6 = call "index_set_1of6"
mz_index_set_2of2 = call "index_set_2of2"
mz_index_set_2of3 = call "index_set_2of3"
mz_index_set_2of4 = call "index_set_2of4"
mz_index_set_2of5 = call "index_set_2of5"
mz_index_set_2of6 = call "index_set_2of6"
mz_index_set_3of3 = call "index_set_3of3"
mz_index_set_3of4 = call "index_set_3of4"
mz_index_set_3of5 = call "index_set_3of5"
mz_index_set_3of6 = call "index_set_3of6"
mz_index_set_4of4 = call "index_set_4of4"
mz_index_set_4of5 = call "index_set_4of5"
mz_index_set_4of6 = call "index_set_4of6"
mz_index_set_5of5 = call "index_set_5of5"
mz_index_set_5of6 = call "index_set_5of6"
mz_index_set_6of6 = call "index_set_6of6"
mz_index_sets_agree = call "index_sets_agree"
mz_length  = call "length"
mz_reverse = call "reverse"
mz_row     = call "row"

-- Array sorting calls
mz_arg_sort = call "arg_sort"
mz_sort     = call "sort"
mz_sort_by  = call "sort_by"

-- Coercion calls
mz_bool2float = call "bool2float"
mz_bool2int   = call "bool2int"
mz_ceil       = call "ceil"
mz_floor      = call "floor"
mz_int2float  = call "int2float"
mz_round      = call "round"
mz_set2array  = call "set2array"

-- String calls
mz_concat    = call "concat"
mz_file_path = call "file_path"
mz_format    = call "format"
mz_join      = call "join"
mz_show      = call "show"
mz_show2d    = call "show2d"
mz_show3d    = call "show3d"
mz_showJSON  = call "showJSON"
mz_show_float = call "show_float"
mz_show_int   = call "show_int"
mz_strig_length = call "string_length"

-- Reflection calls
mz_dom              = call "dom"
mz_dom_array        = call "dom_array"
mz_dom_bounds_array = call "dom_bounds_array"
mz_dom_size         = call "dom_size"
mz_fix              = call "fix"
mz_has_bounds       = call "has_bounds"
mz_has_ub_set       = call "has_ub_set"
mz_is_fixed         = call "is_fixed"
mz_lb               = call "lb"
mz_lb_array         = call "lb_array"
mz_ub               = call "ub"
mz_ub_array         = call "ub_array"

-- Assertions and debugging calls
mz_abort        = call "abort"
mz_assert       = call "assert"
mz_trace        = call "trace"
mz_trace_stdout = call "trace_stdout"

-- Calls for @Enum@s
mz_enum_next = call "enum_next"
mz_enum_prev = call "enum_prev"
mz_to_enum   = call "to_enum"

-- Calls for Optionals

-- Random number generator calls
mz_bernoulli      = call "bernoulli"
mz_binomial       = call "binomial"
mz_cauchy         = call "cauchy"
mz_chisquared     = call "chisquared"
mz_discrete_distribution = call "discrete_distribution"
mz_exponential    = call "exponential"
mz_fdistribution  = call "fdistribution"
mz_gamma          = call "gamma"
mz_lognormal      = call "lognormal"
mz_normal         = call "normal"
mz_poisson        = call "poisson"
mz_tdistribution  = call "tdistribution"
mz_uniform        = call "uniform"
mz_weibull        = call "weibull"

-- Special constraints
mz_implied_constraint           = call "implied_constraint"
mz_redundant_constraint         = call "redundant_constraint"
mz_symmetry_breaking_constraint = call "symmetry_breaking_constraint"

-- Language information
mz_mzn_compiler_version  = call "mzn_compiler_version"
mz_mzn_version_to_string = call "mzn_version_to_string"

-- MiniZinc operators

infixl 3 /\., <->., <-., ->., \/., `_xor_`
infix 4 <., <=., >., >=., =.=, !=.
infix 5 `_in_`, `_subset_`, `_superset_`
infixl 6 `_union_`, `_diff_`, `_symdiff_`, --.
infixl 7 +., -.
infixl 8 *., /., `_div_`, `_mod_`, `_intersect_`, ++.

mz_absent = call "absent"
mz_deopt  = call "deopt"
mz_occurs = call "occurs"
--mz_regular = call "regular"

-- Comparison operators
mz_neq = Op "!="
mz_lt  = Op "<" 
mz_lte = Op "<="
mz_eq  = Op "=" 
mz_gt  = Op ">" 
mz_gte = Op ">="
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
mz_times = Op "*"  
mz_plus  = Op "+"  
mz_minus = Op "-"  
mz_div   = Op "/"  
mz_idiv  = Op "div"
mz_mod   = Op "mod"
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
mz_rarrow  = Op "->"  
mz_and     = Op "/\\" 
mz_larrow  = Op "<-"  
mz_lrarrow = Op "<->" 
mz_not     = Op "not" 
mz_or      = Op "\\/" 
mz_xor     = Op "xor"
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
mz_range     = Op ".."
mz_diff      = Op "diff"
mz_in        = Op "in"
mz_intersect = Op "intersect"
mz_subset    = Op "subset"
mz_superset  = Op "superset"
mz_symdiff   = Op "symdiff"
mz_union     = Op "union"
-- | @..@
(--.)        = Bi mz_range
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
mz_pp = Op "++"
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