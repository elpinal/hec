module Refine.Inter where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import qualified Data.Map.Lazy as Map

import Refine.Parse
import Refine.Type

data Error = Error ErrorType String
  deriving (Eq, Show)

data ErrorType =
    Unbound
  | FixityError
  | TypeMismatch
  deriving (Eq, Show)

emitError :: Monad m => ErrorType -> String -> ExceptT Error m a
emitError t = throwError . Error t

type Env = ExceptT Error (State InterState)

evalEnv :: InterState -> Env a -> Either Error a
evalEnv s = flip evalState s . runExceptT

isDefined :: String -> Env Bool
isDefined name = Map.member name <$> gets table

resolve :: String -> Env (Maybe (Type, Maybe Fixity))
resolve name = Map.lookup name <$> gets table

resolveE :: String -> Env (Type, Maybe Fixity)
resolveE name = resolve name >>= maybe (emitError Unbound $ "not defined: " ++ show name) return

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

class Display a where
  display :: a -> String

data Fixity = Fixity (Maybe Direction) Precedence

instance Display Fixity where
  display (Fixity d n) = maybe "infix" display d ++ " " ++ show n

data Direction =
    LeftAssoc
  | RightAssoc
  deriving Eq

instance Display Direction where
  display LeftAssoc = "infixl"
  display RightAssoc = "infixr"

type Precedence = Int

defaultFixity :: Fixity
defaultFixity = Fixity (Just LeftAssoc) 9

getFixity :: String -> Env Fixity
getFixity name = resolveE name >>= app . first (flip maybe return . setDefault)
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
  e @ (Fixity d p) <- getFixity name
  f <- getFixity name1
  case f of
    (Fixity _ p1)
      | p < p1 -> return x
      | p > p1 -> return swapped
    (Fixity d1 @ (Just LeftAssoc) _)
      | d1 == d -> return x
    (Fixity d1 @ (Just RightAssoc) _)
      | d1 == d -> return swapped
    _ -> emitError FixityError $ "fixity error: cannot mix " ++ show name1 ++ " [" ++ display f ++ "] and " ++ show name ++ " [" ++ display e ++ "]"
  where
    swapped :: Expr
    swapped = BinOp name1 lhs1 $ BinOp name rhs1 rhs
recons x = return x

-- | Convert binary operations to function applications.
binToFun :: Expr -> Expr
binToFun (BinOp name lhs rhs) = App (Var name) (binToFun lhs) `App` binToFun rhs
binToFun (App x y) = App (binToFun x) $ binToFun y
binToFun (Abs name e) = Abs name $ binToFun e
binToFun (Case e xs) = Case (binToFun e) $ map (second binToFun) xs
binToFun (Tuple es) = Tuple $ map binToFun es
binToFun (Record fs) = Record $ map (second binToFun) fs
binToFun e = e

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

-- TODO: Bin should not hold String.
data Bin = Bin String
  deriving (Eq, Show)

data U = U
  deriving (Eq, Show)

data Constant =
    CInt Int
  | CBool Bool
  deriving (Eq, Show)

type Translator = StateT Int (WriterT [ThreeAddress] Env)

translate :: InterState -> Translator a -> Either Error (a, [ThreeAddress])
translate s = evalEnv s . runWriterT . flip evalStateT 0

newTempVar :: Translator Address
newTempVar = state $ TempVar &&& (+ 1)

newLabel :: Translator Address
newLabel = state $ Label &&& (+ 1) -- shares number with TempVar

genThreeAddress :: Expr -> Translator Address

genThreeAddress (Lit lit) = return $ genLit lit

genThreeAddress (BinOp name lhs rhs) = do
  tv <- newTempVar
  l <- genThreeAddress lhs
  r <- genThreeAddress rhs
  tell [BinAssign tv (Bin name) l r]
  return tv

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
  newLabel

genLit :: Literal -> Address
genLit (LitInt n) = Const $ CInt n
genLit (LitBool b) = Const $ CBool b
