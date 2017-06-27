{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Interfaces.MonVar2 where

import qualified Interfaces.MZAST as M
import Interfaces.MZBuiltIns
import Control.Monad.Except
import Data.String

data Assignment = Assignment M.Ident M.AnnExpr

instance Show Assignment where
  show (Assignment x e) = show x ++ " = " ++ show e


data Model = Model { m_imports      :: [String]
                   , m_declarations :: [M.Declaration]
                   , m_assignments  :: [Assignment]
                   , m_constraints  :: [M.AnnExpr]
                   , m_solve        :: Maybe M.Solve
                   , m_output       :: Maybe M.Expr
                   }
  deriving Show

emptyModel = Model { m_imports = []
                   , m_declarations = []
                   , m_assignments = []
                   , m_constraints = []
                   , m_solve = Nothing
                   , m_output = Nothing
                   }

addImport :: Model -> String -> Model
addImport m file = Model { m_imports      = file : m_imports m
                         , m_declarations = m_declarations m
                         , m_assignments  = m_assignments m
                         , m_constraints  = m_constraints m
                         , m_solve        = m_solve m
                         , m_output       = m_output m
                         }

addDeclaration :: Model -> M.Declaration -> Model
addDeclaration m d = Model { m_imports      = m_imports m
                           , m_declarations = d : m_declarations m
                           , m_assignments  = m_assignments m
                           , m_constraints  = m_constraints m
                           , m_solve        = m_solve m
                           , m_output       = m_output m
                           }

addAssignment :: Model -> Assignment -> Model
addAssignment m a = Model { m_imports      = m_imports m
                          , m_declarations = m_declarations m
                          , m_assignments  = a : m_assignments m
                          , m_constraints  = m_constraints m
                          , m_solve        = m_solve m
                          , m_output       = m_output m
                          }

addConstraint :: Model -> M.AnnExpr -> Model
addConstraint m c = Model { m_imports      = m_imports m
                          , m_declarations = m_declarations m
                          , m_assignments  = m_assignments m
                          , m_constraints  = c : m_constraints m
                          , m_solve        = m_solve m
                          , m_output       = m_output m
                          }

setSolve :: Model -> M.Solve -> Model
setSolve m s = Model { m_imports      = m_imports m
                     , m_declarations = m_declarations m
                     , m_assignments  = m_assignments m
                     , m_constraints  = m_constraints m
                     , m_solve        = Just s
                     , m_output       = m_output m
                     }

setOutput :: Model -> M.Expr -> Model
setOutput m o = Model { m_imports      = m_imports m
                      , m_declarations = m_declarations m
                      , m_assignments  = m_assignments m
                      , m_constraints  = m_constraints m
                      , m_solve        = m_solve m
                      , m_output       = Just o}

toModel :: KM e a -> Model
toModel   (Return _)         = emptyModel
toModel   (Include file k)   = addImport (toModel k) file 
toModel   (Comment text k)   = toModel k
toModel   (Declare d k)      = addDeclaration (toModel k) d
toModel a@(Variable t i x k) = addDeclaration (toModel k) (toDeclaration a)
toModel   (Assign x e k)     = addAssignment (toModel k) (Assignment x e)
toModel   (Constrain e k)    = addConstraint (toModel k) e
toModel   (Solve s k)        = setSolve (toModel k) s
toModel   (Output e k)       = setOutput (toModel k) e

toSignature :: KM e a -> M.DeclarationSignature
toSignature (Variable i t x _) = M.Variable (i, t, M.stringToIdent x)

toDeclaration :: KM e a -> M.Declaration
toDeclaration a@(Variable i t x _) = M.Declaration (toSignature a) [] Nothing

{-
toItems :: KM e a -> [Either e M.Item]
toItems   (Return _)         = []
toItems   (Include file k)   = Right (M.Include file)  : toItems k
toItems   (Comment text k)   = Right (M.Comment text)  : toItems k
toItems   (Declare d k)      = Right (M.Declare d)     : toItems k
toItems a@(Variable t i x k) = Right 
  (M.Declare (M.Declaration (toSignature a) [] Nothing)) : toItems k
toItems   (Assign x e k)     = Right (M.Assign x e)    : toItems k
toItems   (Constrain e k)    = Right (M.Constraint e)  : toItems k
toItems   (Solve s k)        = Right (M.Solve s)       : toItems k
toItems   (Output e k)       = Right (M.Output e)      : toItems k
toItems   (Error e)          = [Left e]

instance Monoid Model where
  mempty = emptyModel
  mappent m1 m2 = Model { imports = mappend (imports m1) (imports m2)
                        , delcarations = mappend (declarations m1) (declarations m2)
                        , assignments = mappend (assignments m1) (assignments m2)
                        , constraints = mappend (constraints m1) (constraints m2)
                        , solve = mappend (solve m1) (solve m2)
                        }
-}

type DS = M.DeclarationSignature

data KM e a where
  Return    :: a -> KM e a
  Include   :: String -> KM e a -> KM e a
  Comment   :: String -> KM e a -> KM e a
  Declare   :: M.Declaration -> KM e a -> KM e a
  Variable  :: M.Inst -> M.Type -> String -> KM e a -> KM e a
  Function  :: DS -> KM e a -> KM e a
  Predicate :: DS -> KM e a -> KM e a
  Test      :: DS -> KM e a -> KM e a
  Annot     :: DS -> KM e a -> KM e a
  Assign    :: M.Ident -> M.AnnExpr -> KM e a -> KM e a
  Constrain :: M.AnnExpr -> KM e a -> KM e a
  Solve     :: M.Solve -> KM e a -> KM e a
  Output    :: M.Expr -> KM e a -> KM e a
  Error     :: e -> KM e a

instance (Show e, Show a) => Show (KM e a) where
  show (Return x)         = "" --show x ++ ";\n"
  show (Include text k)   = "include \"" ++ text ++ ";\n" ++ show k
  show (Comment text k)   = "%% " ++ text ++ ";\n" ++ show k
  show (Declare d k)      = show d ++ ";\n" ++ show k
  show (Variable i t x k) = show i ++ " " ++ show t ++ " : " ++ x ++ ";\n" ++ show k
  show (Function d k)     = "function " ++ show d ++ ";\n" ++ show k
  show (Assign x e k)     = show x ++ " = " ++ show e ++ ";\n" ++ show k
  show (Constrain e k)    = "constraint " ++ show e ++ ";\n" ++ show k
  show (Solve s k)        = "solve " ++ show s ++ ";\n" ++ show k
  show (Error e)          = show e

instance Functor (KM e) where
  fmap = liftM

instance Applicative (KM e) where
  pure = Return
  (<*>) = ap

instance Monad (KM e) where
  (Return x)         >>= f = f x
  (Include file k)   >>= f = Include file (k >>= f)
  (Comment text k)   >>= f = Comment text (k >>= f)
  (Declare d k)      >>= f = Declare d (k >>= f)
  (Variable i t x k) >>= f = Variable i t x (k >>= f)
  (Function ds k)    >>= f = Function ds (k >>= f)
  (Predicate ds k)   >>= f = Predicate ds (k >>= f)
  (Test ds k)        >>= f = Test ds (k >>= f)
  (Annot ds k)       >>= f = Annot ds (k >>= f)
  (Assign x e k)     >>= f = Assign x e (k >>= f)
  (Constrain e k)    >>= f = Constrain e (k >>= f)
  (Solve s k)        >>= f = Solve s (k >>= f)
  (Output es k)      >>= f = Output es (k >>= f)
  (Error e)          >>= f = Error e

instance MonadError e (KM e) where
  throwError = Error
  (Error e) `catchError` h = h e
  x         `catchError` _ = x
{-
class ToKM a where
  mkKM :: a -> KM M.Item

instance ToKM M.Item where
  mkKM item@(M.Comment text) = Comment text (Return item)
  mkKM item@(M.Include file) = Include file (Return item)
  mkKM item@(M.Declare d)    = Declare d (Return item)
  mkKM item@(M.Assign x e)   = Assign x e (Return item)
  mkKM item@(M.Constraint e) = Constrain e (Return item)
  mkKM item@(M.Solve s)      = Solve s (Return item)
  mkKM item@(M.Output es)    = Output es (Return item)
  
instance ToKM M.Declaration where
  mkKM d@(M.Declaration ds ans me) = Declare d (Return (M.Declare d))

instance ToKM M.DeclarationSignature where
  mkKM ds@(M.Variable (i, t, x)) = Variable i t (M.identToString x) (Return (M.Declare (M.declareOnly ds)))
  mkKM ds@(M.Function p args)    = Function ds (Return (M.Declare (M.declareOnly ds)))
  mkKM ds@(M.Predicate p args)   = Predicate ds (Return (M.Declare (M.declareOnly ds)))
  mkKM ds@(M.Test p args)        = Test ds (Return (M.Declare (M.declareOnly ds)))
  mkKM ds@(M.Annotation' p args) = Annot ds (Return (M.Declare (M.declareOnly ds)))
-}
(%) text = Comment text (Return ())
include file = Include file (Return ())
constraint e = Constrain (M.toSimpleExpr e) (Return ())
solve s = Solve s (Return ())
output es = Output es (Return ())

var :: M.Type -> String -> KM PossibleError M.Expr
var t x = Variable M.Dec t x (Return (M.Var $ M.Simpl x))

par :: M.Type -> String -> KM PossibleError M.Expr
par t x = Variable M.Par t x (Return (M.Var $ M.Simpl x))

list2KM :: [KM PossibleError M.Expr] -> KM PossibleError [M.Expr]
list2KM [] = Return []
list2KM ls = Return (gather ls)

gather :: [KM PossibleError M.Expr] -> [M.Expr]
gather [] = []
gather ((Return v):xs) = (v :: M.Expr) : gather xs
gather (x:xs)          = gather xs

param :: KM PossibleError M.Expr -> KM PossibleError M.Param
param (Variable i t x _) = Return (i, t, M.stringToIdent x)
--param x                  = Error (NoVarDS x)

params :: [M.Param] -> [KM PossibleError M.Expr] -> KM PossibleError [M.Param]
params ls [] = Return ls
params ls (x:xs) = case param x of
                     Return p -> params (ls ++ [p]) xs
                     Error e  -> Error e

function :: M.Inst -> M.Type -> String -> [KM PossibleError M.Expr] 
         -> KM PossibleError ([M.Expr] -> M.Expr, [M.Expr])
function i t name kmargs = let args = params [] kmargs 
                           in case args of
                                Return ps -> Function
                                              (M.Function (i, t, M.Simpl name) ps)
                                              (Return (prefCall name, gather kmargs))
                                Error e   -> Error e

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

infix 1 =.
class Assignable a b | a -> b where
  (=.) :: a -> M.Expr -> KM PossibleError b

-- plain assignment
-- instance Assignable M.Ident () where
  -- x =. e = Right $ Assign x (M.toSimpleExpr e) (Return ())

instance Assignable M.Expr M.Expr where
  (M.Var x) =. e = Assign x (M.toSimpleExpr e) (Return (M.Var x))
  x         =. e = Error (NoVarDS e)

-- assignment on declaration
{-
instance Assignable (KM PossibleError DS) M.Expr where
  (Variable p) =. e
  ds =. e = ds >>= (\x -> 
            Declare (M.Declaration x [] (Just (M.toSimpleExpr e))) (Return (M.Var (extractName x))))

data DeclarationSignature = Variable Param 
                          | Predicate Ident [Param]
                          | Test Ident [Param]
                          | Function Param [Param]
                          | Annotation' String [Param]
-}
instance Assignable (KM PossibleError M.Expr) M.Expr where
  x =. e = x >>= (=. e)
{-
instance Assignable (KM PossibleError ([M.Expr] -> M.Expr, [M.Expr])) where
  x =. e = x >>= (\(f, xs) -> 
             )
-}
instance Assignable (KM PossibleError M.Ident) M.Expr where
  x =. e = x >>= (\a -> Assign a (M.toSimpleExpr e) (Return (M.Var a)))

extractName :: DS -> M.Ident
extractName (M.Variable (_, _, x))   = x
extractName (M.Function (_, _, x) _) = x
extractName (M.Predicate x _)        = x
extractName (M.Test x _)             = x
extractName (M.Annotation' x _)      = M.Simpl x

-- Handling errors
code :: String -> String
code str = "`" ++ str ++ "'"
{-
instance MonadError PossibleError KM where
  throwError (AssignmentToExpr e) = Error
-}
data PossibleError
  = AssignmentToExpr M.Expr -- Assigning to an expression instead of a variable
  | NoVarDS M.Expr

instance Show PossibleError where
  show (AssignmentToExpr e) 
    = "Error: Attempt to assign to " ++ code (show e) 
    ++ " :: Expr.\nAssignments are allowed on " ++ code "Ident" 
    ++ " values only and exceptionally on " ++ code "Expr" 
    ++ " values created with the " ++ code "Var" ++ " constructor."
  
assign :: M.Expr -> M.Expr -> Either PossibleError (KM PossibleError Assignment)
assign (M.Var x) e = Right $ 
  Assign x (M.toSimpleExpr e) (Return (Assignment x (M.toSimpleExpr e)))
assign x         _ = Left  $ AssignmentToExpr x