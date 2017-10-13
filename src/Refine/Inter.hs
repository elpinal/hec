module Refine.Inter where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import qualified Data.Map.Lazy as Map

import Refine.Parse

type Env = ExceptT String (State InterState)

evalEnv :: InterState -> Env a -> Either String a
evalEnv s = flip evalState s . runExceptT

isDefined :: String -> Env Bool
isDefined name = Map.member name <$> gets table

resolve :: String -> Env (Maybe (Type, Maybe Fixity))
resolve name = Map.lookup name <$> gets table

resolveE :: String -> Env (Type, Maybe Fixity)
resolveE name = resolve name >>= maybe (throwError $ "not defined: " ++ show name) return

newTypeVar :: Env Type
newTypeVar = do
  s <- get
  put s { supply = supply s + 1 }
  return . TypeVar $ "a" ++ show (supply s)

data InterState = InterState
  { table :: SymbolTable
  , supply :: Int
  }

interState :: InterState
interState = InterState
  { table = Map.empty
  , supply = 0
  }

type SymbolTable = Map.Map String (Type, Maybe Fixity)

data Fixity = Fixity (Maybe Direction) Precedence

data Direction =
    LeftAssoc
  | RightAssoc
  deriving Eq

type Precedence = Int

defaultFixity :: Fixity
defaultFixity = Fixity (Just LeftAssoc) 9

getFixity :: String -> Env Fixity
getFixity name = do
  resolveE name >>= (app <<< flip maybe return . setDefault *** id)
  where
    setDefault :: Type -> Env Fixity
    setDefault t = do
      s <- get
      put s { table = Map.insert name (t, Just defaultFixity) $ table s }
      return defaultFixity

-- Assumes that all the binary operations of the input is defaulted to
-- left-associative and the same precedence.
recons :: Expr -> Env Expr
recons x @ (BinOp name (BinOp name1 lhs1 rhs1) rhs) = do
  (Fixity d p) <- getFixity name
  f <- getFixity name1
  case f of
    (Fixity _ p1)
      | p < p1 -> return x
      | p > p1 -> return swap
    (Fixity d1 @ (Just LeftAssoc) _)
      | d1 == d -> return x
    (Fixity d1 @ (Just RightAssoc) _)
      | d1 == d -> return swap
    (Fixity d1 @ Nothing _)
      | d1 == d -> throwError "cannot associate two non-associative operators"
    otherwise -> throwError $ "cannot mix " ++ show name1 ++ " and " ++ show name
  where
    swap :: Expr
    swap = BinOp name1 lhs1 $ BinOp name rhs1 rhs
recons x = return x

data Type =
    TypeInt
  | TypeBool
  | TypeChar
  | TypeString
  | TypeFun Type Type
  | TypeVar String
  deriving (Eq, Show)

type Subst = Map.Map String Type

apply :: Subst -> Type -> Type
apply s t @ (TypeVar name) = Map.findWithDefault t name s
apply s (TypeFun a b) = TypeFun (apply s a) (apply s b)
apply _ t = t

typeOf :: Expr -> Env Type
typeOf (Lit lit) = return $ litType lit

typeOf (BinOp name lhs rhs) = do
  op <- resolveE name
  l <- typeOf lhs
  r <- typeOf rhs
  case fst op of
    TypeFun a (TypeFun b c)
      | a == l && b == r -> return c
    _ -> throwError "type mismatch"

typeOf (App f x) = do
  ft <- typeOf f
  xt <- typeOf x
  case ft of
    TypeFun a b | a == xt -> return b
    _ -> throwError $ "expected function type which takes " ++ show xt ++ ", but got " ++ show ft

typeOf (Var name) = fst <$> resolveE name

typeOf (Abs name body) = do
  tv <- newTypeVar
  s <- get
  put s { table = Map.insert name (tv, Nothing) $ table s }
  t <- typeOf body
  modify $ \ss -> ss { table = table s }
  return $ TypeFun tv t

litType :: Literal -> Type
litType (LitInt _) = TypeInt
litType (LitBool _) = TypeBool
litType (LitChar _) = TypeChar
litType (LitString _) = TypeString

data ThreeAddress =
    BinAssign Address Bin Address Address -- dest op lhs rhs
  | UAssign Address U Address -- dest op opearand
  | Copy Address Address -- dest src
  | Goto Label
  | Call Address Address -- dest function
  | Param Address
  | Begin
  | End
  | Return Address
  deriving (Eq, Show)

data Address =
    Name String
  | Const Constant
  | TempVar Int
  | Label Label
  deriving (Eq, Show)

type Label = Int

data Bin = Bin String
  deriving (Eq, Show)

data U = U
  deriving (Eq, Show)

data Constant =
    CInt Int
  | CBool Bool
  deriving (Eq, Show)

type Translator = StateT Int (WriterT [ThreeAddress] Env)

translate :: InterState -> Translator a -> Either String (a, [ThreeAddress])
translate s = evalEnv s . runWriterT . flip evalStateT 0

newTempVar :: Translator Address
newTempVar = do
  n <- get
  put $ n + 1
  return $ TempVar n

newLabel :: Translator Address
newLabel = do
  n <- get
  put $ n + 1
  return $ Label n -- shares number with TempVar

genThreeAddress :: Expr -> Translator Address

genThreeAddress (Lit lit) = return $ genLit lit

genThreeAddress (BinOp name lhs rhs) = do
  tv <- newTempVar
  l <- genThreeAddress lhs
  r <- genThreeAddress rhs
  tell [BinAssign tv (Bin name) l r]
  return $ tv

genThreeAddress (App a b) = do
  tv <- newTempVar
  t <- genThreeAddress a
  u <- genThreeAddress b
  tell [Param u, Call tv t]
  return tv

genThreeAddress (Var name) = return $ Name name

-- FIXME: Take care binding.
genThreeAddress (Abs name body) = do
  tell [Begin]
  t <- genThreeAddress body
  tell [Return t, End]
  l <- newLabel
  return l

genLit :: Literal -> Address
genLit (LitInt n) = Const $ CInt n
genLit (LitBool b) = Const $ CBool b
