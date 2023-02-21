module DeBruijn.Internal.Env (
    Env (EmptyEnv, (:>), UnsafeEnv),
    lookupEnv,
    sizeEnv, 
) where

import Data.Kind (Type)
import Data.SkewList.Strict (SkewList)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.SkewList.Strict as SL

import DeBruijn.Ctx
import DeBruijn.Internal.Idx
import DeBruijn.Internal.Size

type Env :: Ctx -> Type -> Type
type role Env nominal representational
newtype Env ctx a = UnsafeEnv { unEnv :: SkewList a }

-------------------------------------------------------------------------------
-- Pattern synonyms
-------------------------------------------------------------------------------


-- We need a GADT to implement pattern synonyms.
type Env' :: Ctx -> Type -> Type
type role Env' nominal representational
data Env' ctx a where
    EmptyEnv' :: Env' EmptyCtx a
    (:>>)     :: Env ctx a -> a -> Env' (S ctx) a

upEnv :: Env ctx a -> Env' ctx a
upEnv (UnsafeEnv env) = case SL.uncons env of
    Nothing      -> unsafeCoerce EmptyEnv'
    Just (x, xs) -> unsafeCoerce (UnsafeEnv xs :>> x)

pattern EmptyEnv :: () => (ctx ~ EmptyCtx) => Env ctx a
pattern EmptyEnv <- (upEnv -> EmptyEnv')
  where EmptyEnv = UnsafeEnv SL.Nil

infixr 5 :>
pattern (:>) :: () => (ctx ~ S ctx') => Env ctx' a -> a -> Env ctx a
pattern xs :> x <- (upEnv -> xs :>> x)
  where xs :> x = UnsafeEnv (SL.cons x (unEnv xs))

{-# COMPLETE EmptyEnv, (:>) #-}

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

deriving instance Functor (Env ctx)
deriving instance Foldable (Env ctx)
deriving instance Traversable (Env ctx)
deriving instance Show a => Show (Env ctx a)

-- | Lookup in the context.
lookupEnv :: Idx ctx -> Env ctx a -> a
lookupEnv (UnsafeIdx i) (UnsafeEnv xs) = xs SL.! i

-- | Size of the environment.
sizeEnv :: Env n a -> Size n
sizeEnv (UnsafeEnv xs) = UnsafeSize (length xs)
