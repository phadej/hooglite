module DeBruijn.Env (
    Env (EmptyEnv, (:::)),
    lookupEnv,
    varEnv,
    sizeEnv, 
) where

import Data.Kind (Type)

import DeBruijn.Ctx
import DeBruijn.Idx
import DeBruijn.Size

type Env :: Ctx -> Type -> Type
data Env ctx a where
    EmptyEnv :: Env EmptyCtx a
    (:::)    :: a -> Env ctx a -> Env (S ctx) a

infixr 5 :::

deriving instance Functor (Env ctx)
deriving instance Foldable (Env ctx)
deriving instance Traversable (Env ctx)
deriving instance Show a => Show (Env ctx a)

-- | Lookup in the context.
lookupEnv :: Var ctx -> Env ctx a -> a
lookupEnv VZ     (x ::: _)  = x
lookupEnv (VS n) (_ ::: xs) = lookupEnv n xs

varEnv :: Size n -> Env n (Var n)
varEnv SZ     = EmptyEnv
varEnv (SS n) = VZ ::: fmap VS (varEnv n)

-- | Size of the environment.
sizeEnv :: Env n a -> Size n
sizeEnv EmptyEnv   = SZ
sizeEnv (_ ::: xs) = SS (sizeEnv xs)
