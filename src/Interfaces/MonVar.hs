{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Interfaces.MonVar where

import Interfaces.MZASTBase hiding (Item(..))
import Interfaces.MZBuiltIns
import Control.Monad.Except
import Data.String

-- * Models


-- TOM: What is their role?

data Model = Model { m_imports      :: [String]
                   , m_declarations :: [Declaration]
                   , m_assignments  :: [Assignment]
                   , m_constraints  :: [Constraint]
                   , m_solve        :: Maybe Solve
                   , m_output       :: [Output]
                   }
  deriving Show

emptyModel :: Model
emptyModel = Model [] [] [] [] Nothing []

addImport :: Model -> String -> Model
addImport m file = Model (file : m_imports m)
                         (m_declarations m)
                         (m_assignments m)
                         (m_constraints m)
                         (m_solve m)
                         (m_output m)

addDeclaration :: Model -> Declaration -> Model
addDeclaration m d = Model (m_imports m)
                           (d : m_declarations m)
                           (m_assignments m)
                           (m_constraints m)
                           (m_solve m)
                           (m_output m)

addAssignment :: Model -> Assignment -> Model
addAssignment m a = Model (m_imports m)
                          (m_declarations m)
                          (a : m_assignments m)
                          (m_constraints m)
                          (m_solve m)
                          (m_output m)

addConstraint :: Model -> Constraint -> Model
addConstraint m c = Model (m_imports m)
                          (m_declarations m)
                          (m_assignments m)
                          (c : m_constraints m)
                          (m_solve m)
                          (m_output m)

setSolve :: Model -> Solve -> Model
setSolve m s = Model (m_imports m)
                     (m_declarations m)
                     (m_assignments m)
                     (m_constraints m)
                     (Just s)
                     (m_output m)

addOutput :: Model -> Output -> Model
addOutput m o = Model (m_imports m)
                      (m_declarations m)
                      (m_assignments m)
                      (m_constraints m)
                      (m_solve m)
                      (o : m_output m)

instance Monoid (Either PossibleError Model) where
  mempty = Right emptyModel
  mappend (Right m1) (Right m2) =
    let imports = mappend (m_imports m1) (m_imports m2)
        declars = mappend (m_declarations m1) (m_declarations m2)
        assigns = mappend (m_assignments m1) (m_assignments m2)
        constrs = mappend (m_constraints m1) (m_constraints m2)
        outputs = mappend (m_output m1) (m_output m2)
    in case (m_solve m1, m_solve m2) of
       (Just _, Just _) -> Left MultipleSolveItems
       (Just s1, Nothing) ->
         Right $ Model imports declars assigns constrs (Just s1) outputs
       (Nothing, Just s2) ->
         Right $ Model imports declars assigns constrs (Just s2) outputs
       (Nothing, Nothing) ->
         Right $ Model imports declars assigns constrs Nothing outputs
  mappend (Right _) (Left err) = Left err
  mappend (Left err) (Right _) = Left err
  mappend (Left er1) (Left _) = Left er1
         
         
type DS = DeclarationSignature
data Assignment = Assignment Ident AnnExpr
  deriving Show

newtype Constraint = Constraint AnnExpr
  deriving Show

newtype Output = Output Expr
  deriving Show

-- * The |KM| free monad
-- | This monad represents the monadic syntax
--   of high-level models.

data KM a where
  Return     :: a -> KM a
  MInclude   :: String -> KM a -> KM a
  MComment   :: String -> KM a -> KM a
  MDeclare   :: Declaration -> KM a -> KM a
  MVariable  :: Inst -> Type -> String -> KM a -> KM a
  MFunction  :: Inst -> Type -> String -> [KM Expr] -> ([Expr] -> Expr) 
             -> (([Expr] -> Expr) -> KM a) -> KM a
  MPredicate :: String -> [KM Expr] -> ([Expr] -> Expr) -> (([Expr] -> Expr) -> KM a) -> KM a
  MTest      :: DS -> KM a -> KM a
  MAnnot     :: DS -> KM a -> KM a
  MAssign    :: Ident -> AnnExpr -> KM a -> KM a
  MConstrain :: AnnExpr -> KM a -> KM a
  MSolve     :: Solve -> KM a -> KM a
  MOutput    :: Expr -> KM a -> KM a
  MError     :: PossibleError -> KM a

instance Functor KM where
  fmap = liftM

instance Applicative KM where
  pure = Return
  (<*>) = ap

instance Monad KM where
  (Return x)         >>= f        = f x
  (MInclude file k)   >>= f       = MInclude file (k >>= f)
  (MComment text k)   >>= f       = MComment text (k >>= f)
  (MDeclare d k)      >>= f       = MDeclare d (k >>= f)
  (MVariable i t x k) >>= f       = MVariable i t x (k >>= f)
  (MFunction i t n ps b k) >>= f  = MFunction i t n ps b (k >=> f)
  (MPredicate n ps b k)    >>= f  = MPredicate n ps b (k >=> f)
  (MTest ds k)        >>= f       = MTest ds (k >>= f)
  (MAnnot ds k)       >>= f       = MAnnot ds (k >>= f)
  (MAssign x e k)     >>= f       = MAssign x e (k >>= f)
  (MConstrain e k)    >>= f       = MConstrain e (k >>= f)
  (MSolve s k)        >>= f       = MSolve s (k >>= f)
  (MOutput es k)      >>= f       = MOutput es (k >>= f)
  (MError err)        >>= f       = MError err

instance MonadError PossibleError KM where
  throwError = MError
  (MError ps) `catchError` f = f ps
  other       `catchError` _ = other

-- ** Convert a high-level |KM| program into a low-level |Model|

safeExtract :: KM b -> Either PossibleError Model
safeExtract = fmap reverseModel . safeAdd emptyModel
  where
    reverseModel :: Model -> Model
    reverseModel m = Model (reverse (m_imports m))
                           (reverse (m_declarations m))
                           (reverse (m_assignments m))
                           (reverse (m_constraints m))
                           (m_solve m)
                           (reverse (m_output m))

safeAdd :: Model -> KM a -> Either PossibleError Model
safeAdd m   (Return _)          = return m
safeAdd m   (MInclude file k)   = safeAdd (addImport m file) k
safeAdd m   (MComment text k)   = safeAdd m k
safeAdd m   (MDeclare d k)      = safeAdd (addDeclaration m d) k
safeAdd m v@(MVariable i t x k) = 
  do d <- toDeclaration v
     safeAdd (addDeclaration m d) k
safeAdd m   (MAssign x e k)          = safeAdd (addAssignment m (Assignment x e)) k
safeAdd m   (MConstrain e k)         = safeAdd (addConstraint m (Constraint e)) k
safeAdd m   (MSolve s k)             = safeAdd (setSolve m s) k
safeAdd m   (MError err)             = Left err
safeAdd m   (MFunction i t n ps b k) = 
  do name <- mapError IncorrectIdentifier (safeStrToIdent n)
     let fparam = (i, t, name)
     params_vars <- mapM extractParam ps
     let (params,vars) = unzip params_vars
     safeAdd 
       (addDeclaration m 
          (Declaration 
             (Function fparam params)   	-- signature
             []                 		-- annotations
             (Just (toSimpleExpr (b vars)))     -- function body
          )
       )
       (k (\as -> Call name (map toSimpleExpr as)))
safeAdd m (MPredicate n ps b k) =
  do name <- mapError IncorrectIdentifier (safeStrToIdent n)
     params_vars <- mapM extractParam ps
     let (params,vars) = unzip params_vars
     safeAdd 
       (addDeclaration m 
          (Declaration 
             (Predicate name params) 	  	-- signature
             []                 		-- annotations
             (Just (toSimpleExpr (b vars)))     -- function body
          )
       )
       (k (\as -> Call name (map toSimpleExpr as)))
safeAdd m (MTest ds k)     = safeAdd (addDeclaration m (Declaration ds [] Nothing)) k
safeAdd m (MAnnot ds k)    = safeAdd (addDeclaration m (Declaration ds [] Nothing)) k
safeAdd m (MOutput e k)    = safeAdd (addOutput m (Output e)) k

extractParam :: KM Expr -> Either PossibleError (Param,Expr)
extractParam (MVariable pinst ptype pn (Return exp@(Var _)))
  = do pname <- mapError IncorrectIdentifier (safeStrToIdent pn)
       return ((pinst,ptype,pname),exp)
extractParam (MError err)
  = Left err
extractParam _
  = Left NotAValidParamater

toSignature :: KM a -> Either IdentError DeclarationSignature
toSignature (MVariable i t x _) = 
  do y <- safeStrToIdent x
     return (Variable (i, t, y))

toDeclaration :: KM a -> Either PossibleError Declaration
toDeclaration a@(MVariable i t x _) = 
  do ds <- mapError IncorrectIdentifier (toSignature a)
     return (Declaration ds [] Nothing)

-- ** Smart Constructors

function  :: Inst -> Type -> String -> [KM Expr] -> ([Expr] -> Expr) 
            -> KM ([Expr] -> Expr)
function i t name params body
   = MFunction i t name params body Return

predicate :: String -> [KM Expr] -> ([Expr] -> Expr) -> KM ([Expr] -> Expr)
predicate name params body 
  = MPredicate name params body Return
 
(%) :: String -> KM ()
(%) text = MComment text (Return ())

include :: String -> KM ()
include file = MInclude file (Return ())

constraint :: Expr -> KM ()
constraint e = MConstrain (toSimpleExpr e) (Return ())

solve :: Solve -> KM ()
solve s = MSolve s (Return ())

output :: Expr -> KM ()
output e = MOutput e (Return ())

-- | Finilizes the representation of a non-annotated solve item. Use '|:' operator to
-- annotate it.
satisfy :: Solve
satisfy = Satisfy []

-- | Finilizes the representation of a non-annotated solve item. Use '|:' operator to
-- annotate it.
minimize :: Expr -> Solve
minimize = Minimize []

-- | Finalizes the representation of a non-annotated solve item. Use '|:' operator to
-- annotate it.
maximize :: Expr -> Solve
maximize = Maximize []

var :: Type -> String -> KM Expr
var t x = do y <- safeIdent x
             MVariable Dec t (identToString y) (Return (Var y))

par :: Type -> String -> KM Expr
par t x = do y <- safeIdent x
             MVariable Par t (identToString y) (Return (Var y))

infix 1 =.
class Assignable a b | a -> b where
  (=.) :: a -> Expr -> KM b

-- plain assignment
-- instance Assignable Ident () where
  -- x =. e = Right $ Assign x (toSimpleExpr e) (Return ())

instance Assignable Expr () where
  (Var x) =. e = MAssign x (toSimpleExpr e) (Return ())
  _       =. e = MError (AssignmentToExpr e)

-- assignment on declaration
instance Assignable (KM Expr) Expr where
  (MVariable i t x (Return (Var n))) =. e = 
      do n <- safeIdent x
         MDeclare
           (Declaration (Variable (i, t, n)) [] (Just (toSimpleExpr e)))
           (Return (Var n))

{-
extractName :: DS -> Ident
extractName (Variable (_, _, x))   = x
extractName (Function (_, _, x) _) = x
extractName (Predicate x _)        = x
extractName (Test x _)             = x
extractName (Annotation' x _)      = Simpl x
-}
-- Handling errors
code :: String -> String
code str = "`" ++ str ++ "'"
{-
instance MonadError PossibleError KM where
  throwError (AssignmentToExpr e) = Error
-}
data PossibleError
  = IncorrectIdentifier (IdentError)
  | AssignmentToExpr Expr -- Assigning to an expression instead of a variable
  | NoVarDS DS
  | MultipleSolveItems
  | NotAllowedInLetDef
  | NotAValidParamater
  deriving Show

{-
instance Show PossibleError where
  show (AssignmentToExpr e) 
    = "Error: Attempt to assign to " ++ code (show e) 
    ++ " :: Expr.\nAssignments are allowed on " ++ code "Ident" 
    ++ " values only and exceptionally on " ++ code "Expr" 
    ++ " values created with the " ++ code "Var" ++ " constructor."
-}  
assign :: Expr -> Expr -> Either PossibleError (KM Assignment)
assign (Var x) e = Right $ 
  MAssign x (toSimpleExpr e) (Return (Assignment x (toSimpleExpr e)))
assign x         _ = Left  $ AssignmentToExpr x

declareOnly :: DeclarationSignature -> Declaration
declareOnly ds = Declaration ds [] Nothing

stopIdentError :: IdentError -> KM a
stopIdentError = MError . IncorrectIdentifier

safeIdent :: String -> KM Ident
safeIdent s = case safeStrToIdent s of
                Right x  -> return x
                Left err -> stopIdentError err

-- Expressions (plain)
__ :: Expr
__ = AnonVar

-- | MiniZinc boolean constant @true@.
true :: Expr
true = BConst True

-- | MiniZinc boolean constant @false@.
false :: Expr
false = BConst False

-- | Used to represent a MiniZinc integer constant.
-- Example:
--
-- >>> constraint $ var "x" !=. int 1
-- constraint x != 1;
int :: Int -> Expr
int = IConst

-- | Used to represent a MiniZinc float constant.
float :: Float -> Expr
float = FConst

-- | Used to represent a MiniZinc string constant.
string :: String -> Expr
string = SConst

-- boolSet :: [Bool] -> Expr
-- boolSet = SetLit . (map BConst)

-- | Used to represent a MiniZinc set of integers.
intSet :: [Int] -> Expr
intSet = mapSet IConst

-- | Used to represent a MiniZinc set of floats.
floatSet :: [Float] -> Expr
floatSet = mapSet FConst

-- | Used to represent a MiniZinc set of strings.
stringSet :: [String] -> Expr
stringSet = mapSet SConst

-- | Used to represent a MiniZinc set. In @mapSet f ls@, the elements of the MiniZinc 
-- set are represented by the resulting 'Expr's after applying @f@ on the elements of 
-- @ls@.
mapSet :: (a -> Expr) -> [a] -> Expr
mapSet f = SetLit . map f

-- | prop> set = SetLit
set :: [Expr] -> Expr
set = SetLit

-- | Used to represent a MiniZinc array of booleans.
boolArray :: [Bool] -> Expr
boolArray = mapArray BConst

-- | Used to represent a MiniZinc array of integers.
intArray :: [Int] -> Expr
intArray = mapArray IConst

-- | Used to represent a MiniZinc array of floats.
floatArray :: [Float] -> Expr
floatArray = mapArray FConst

-- | Used to represent a MiniZinc array of strings.
stringArray :: [String] -> Expr
stringArray = mapArray SConst

-- | Used to represent a 2-dimensional MiniZinc array of booleans.
boolArray2 :: [[Bool]] -> Expr
boolArray2 = mapArray2 BConst

-- | Used to represent a 2-dimensional MiniZinc array of integers.
intArray2 :: [[Int]] -> Expr
intArray2 = mapArray2 IConst

-- | Used to represent a 2-dimensional MiniZinc array of floats.
floatArray2 :: [[Float]] -> Expr
floatArray2 = mapArray2 FConst

-- | Used to represent a 2-dimensional MiniZinc array of strings.
stringArray2 :: [[String]] -> Expr
stringArray2 = mapArray2 SConst

-- | Represents a one-dimensional MiniZinc array by mapping.
mapArray :: (a -> Expr) -> [a] -> Expr
mapArray f = ArrayLit . map f

-- | @mapArray2 f lss@ represents a two-dimensional MiniZinc array by mapping @f@ to all
-- elements of all lists in @lss@.
mapArray2 :: (a -> Expr) -> [[a]] -> Expr
mapArray2 f = ArrayLit2D . (map (map f))

-- | prop> array = ArrayLit
array :: [Expr] -> Expr
array = ArrayLit

-- | prop> array2 = ArrayLit2
array2 :: [[Expr]] -> Expr
array2 = ArrayLit2D

-- Comprehension

infix 2 #/., #|.
-- | Creates the representation of a MiniZinc set comprehension. In @expr #/. cts@, 
-- @expr@ represents the head expression of the set comprehension and @cts@ is a list of 
-- its generator expressions' representations.
-- 
-- Example: 
-- 
-- >>> int 2 *. var "i" #/. [["i"] @@ int 0 ... int 5]
-- {2 * i | i in 0 .. 5}
(#/.) :: Expr -> [CompTail] -> Expr
e #/. cts = SetComp e (mergeCompTails cts)

-- | Similar to '#/.' for array comprehensions.
(#|.) :: Expr -> [CompTail] -> Expr
e #|. cts = ArrayComp e (mergeCompTails cts)

infix 9 !.
-- | Represents a MiniZinc array access.
-- 
-- Exaamples:
-- 
-- >>> "array"!.[int 1]
-- array[1]
-- 
-- >>> "matrix"!.[var "i", var "j"]
-- matrix[i,j]
(!.) :: Expr   -- ^ Array's name
     -> [Expr] -- ^ Indexes of the desired element
     -> Expr
(!.) = ArrayElem
--x !. is = ArrayElem x is

-- | Used to construct the representation of a comprehension tail with a single generator 
-- expression. See the example in the documentation for '#/.'.
infix 5 @@
(@@) :: [String] -> Expr -> CompTail
(@@) vars e = ([(map Simpl vars, e)], Nothing)

--infixl 6 `and_`
combineCompTail :: CompTail -> CompTail -> CompTail
combineCompTail (ins1, me1) (ins2, me2) = (ins1 ++ ins2, decideWhere me1 me2)

mergeCompTails :: [CompTail] -> CompTail
mergeCompTails = foldr1 combineCompTail

decideWhere :: Maybe Expr -> Maybe Expr -> Maybe Expr
decideWhere Nothing   Nothing   = Nothing
decideWhere Nothing   (Just e)  = Just e
decideWhere (Just e)  Nothing   = Just e
decideWhere (Just e1) (Just e2) = Just $ Bi (Op (Simpl "/\\")) e1 e2

infix 4 `where_`
-- | Adds a representation for a MiniZinc @where@ clause in a generator expression.
--
-- Example:
--
-- >>> var "i" *. var "j" #/. [["i", "j"] @@ int 0 ... int 5 `where_` (var "i" !=. var "j")]
-- {i * j | i, j in 0 .. 5 where i != j}
where_ :: CompTail -> Expr -> CompTail
where_ (gs, _) e = (gs, Just e)

-- Generator calls
-- CompTail list makes no sense to be empty
-- | Used for the representation of a generator call.
--
-- Examples:
--
-- >>> forall [["i"] @@ var "S", ["j"] @@ var "S"] "sum" ("x"!.[var"i", var "j"]) =.= var "y"
-- sum(i in S, j in S) (x[i, j]) = y
--
-- >>> forall [["c"] @@ var "C"] "forall" (
-- >>>     forall [["s"] @@ var "S"] "sum" (mz_bool2int["bs"!.[var "s"] =.= var "c"])
-- >>> =.= "result"!.[var "c"])
-- forall(c in C) (sum(s in S) (bool2int(bs[s] = c)) = result[c])

forall :: [CompTail] -- ^ Generator expressions' representation
       -> String     -- ^ The name of the called operation
       -> Expr       -- ^ The head expression of the underlying array comprehension
       -> Expr
forall cts name e = GenCall (Simpl name) (mergeCompTails cts) e

-- Constrained types
-- | Represents a constrained type defined by a set parameter.
--
-- Example:
--
-- >>> declare $ variable Dec Int "one2three" =. intSet [1, 2, 3]
-- var int: one2three = {1, 2, 3};
-- 
-- >>> declare $ variable Dec (ctvar "one2three") "x"
-- var one2three: x;
ctvar :: String -> Type
ctvar = CT . Var . Simpl

ct :: Expr -> Type
ct = CT

-- | Represents a type variable.
($$) :: String -> Type
($$) = VarType

-- | Used together with 'then_' and 'elseif_' \/ 'else_' to represent an if-then-else 
-- MiniZinc expression. In case of multiple alternatives, use 'elseif_', but the last 
-- alternative should be represented with the use of 'else_'.
-- 
-- Example:
--
-- >>> if_ true `then_` int 1 `else_` int 0
-- if true then 1 else 0 endif;
if_ :: Expr -> (Expr -> [(Expr, Expr)])
if_ e = \e1 -> [(e, e1)]

-- | cf. 'if_'
then_ :: (Expr -> [(Expr, Expr)]) -> Expr -> [(Expr, Expr)]
then_ f e = f e

-- | cf. 'if_'
elseif_ :: [(Expr, Expr)] -> Expr -> (Expr -> [(Expr, Expr)])
elseif_ es e = \e1 -> es ++ [(e, e1)]

-- | cf. 'if_'
else_ :: [(Expr, Expr)] -> Expr -> Expr
else_ = ITE

-- Let expressions
data PModel = PModel { pm_declarations :: [Declaration]
                     , pm_constraints :: [Constraint]
                     }

data LetModel = LetModel PModel AnnExpr

instance Monoid PModel where
  mempty = emptyPModel
  mappend m1 m2 = PModel (mappend (pm_declarations m1) (pm_declarations m2))
                         (mappend (pm_constraints m1) (pm_constraints m2))

addPDeclaration :: PModel -> Declaration -> PModel
addPDeclaration m d = PModel (d : pm_declarations m) (pm_constraints m)

addPConstraint :: PModel -> Constraint -> PModel
addPConstraint m c = PModel (pm_declarations m) (c : pm_constraints m)

instance Monoid (Either PossibleError PModel) where
  mempty = Right $ PModel [] []
  
  mappend (Right m1) (Right m2) = Right $ mappend m1 m2
  mappend (Right m1) (Left err) = Left err
  mappend (Left err) (Right m2) = Left err
  mappend (Left er1) (Left er2) = Left er1

-- instance HasItems PModel where
emptyPModel = PModel [] []

reversePModel :: PModel -> PModel
reversePModel m = PModel (reverse $ pm_declarations m)
                         (reverse $ pm_constraints m)

safePAdd :: PModel -> KM a -> Either PossibleError PModel
safePAdd m   (Return _)          = Right $ reversePModel m
safePAdd m   (MInclude file k)   = Left NotAllowedInLetDef
safePAdd m   (MComment c k)      = Left NotAllowedInLetDef
safePAdd m   (MDeclare d k)      = safePAdd (addPDeclaration m d) k
safePAdd m v@(MVariable t i x k) = case toDeclaration v of
                                     Right d  -> safePAdd (addPDeclaration m d) k
                                     Left err -> Left err
-- safeAdd m   (MFunction t i x 
-- safePAdd m v@(MPredicate d k)    = case toDeclaration v of
--                                      Right d  -> safePAdd (addPDeclaration m d) k
--                                      Left err -> Left err
safePAdd m v@(MTest d k)         = case toDeclaration v of
                                     Right d  -> safePAdd (addPDeclaration m d) k
                                     Left err -> Left err
safePAdd m v@(MAnnot d k)        = case toDeclaration v of
                                     Right d  -> safePAdd (addPDeclaration m d) k
                                     Left err -> Left err
safePAdd m v@(MAssign x e k)     = Left NotAllowedInLetDef
safePAdd m   (MConstrain c k)    = safePAdd (addPConstraint m (Constraint c)) k
safePAdd m   (MSolve s k)        = Left NotAllowedInLetDef
safePAdd m   (MOutput o k)       = Left NotAllowedInLetDef
safePAdd m   (MError err)        = Left err

safePExtract :: KM a -> Either PossibleError PModel
safePExtract = safePAdd emptyPModel

let_ :: KM a -> ([Expr] -> KM ()) -> KM ()
let_ x f = undefined

instance IsString Expr where
  fromString = Var . stringToIdent

instance Num Expr where
  fromInteger = int . fromIntegral
  (+) = (+.)
  (*) = (*.)
  (-) = (-.)
  abs a = mz_abs [a]
  signum a =
    if_ (a <. 0) `then_` (-1)
    `elseif_` (a >. 0) `then_` 1
    `else_` 0

mapError :: (e -> e') -> Either e a -> Either e' a
mapError f (Left e)   =  Left (f e)
mapError f (Right x)  =  Right x
