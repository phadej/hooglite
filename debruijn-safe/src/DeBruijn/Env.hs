module DeBruijn.Env (
    Env (EmptyEnv, (:>)),
    lookupEnv,
    sizeEnv, 
) where

import Data.Kind (Type)

import DeBruijn.Ctx
import DeBruijn.Idx
import DeBruijn.Size

type Env :: Ctx -> Type -> Type
data Env ctx a where
    EmptyEnv :: Env EmptyCtx a
    (:>)     :: Env ctx a -> a -> Env (S ctx) a

infixr 5 :>

deriving instance Functor (Env ctx)
deriving instance Foldable (Env ctx)
deriving instance Traversable (Env ctx)
deriving instance Show a => Show (Env ctx a)

-- | Lookup in the context.
lookupEnv :: Idx ctx -> Env ctx a -> a
lookupEnv IZ     (_  :> x)  = x
lookupEnv (IS n) (xs :> _) = lookupEnv n xs

-- | Size of the environment.
sizeEnv :: Env n a -> Size n
sizeEnv EmptyEnv  = SZ
sizeEnv (xs :> _) = SS (sizeEnv xs)
