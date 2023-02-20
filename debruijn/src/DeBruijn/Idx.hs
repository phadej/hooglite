{-# LANGUAGE CPP #-}
-- | Variables for well-scoped terms.
module DeBruijn.Idx (
    -- * Variables
    Var (VZ,VS),
    absurdVar,
    unvar,
    indexVar,
    -- ** Common patterns
    unusedVar,
    unusedVar2,
    unusedVar3,
) where

import Data.Kind            (Type)
import DeBruijn.Ctx

#ifdef SAFE
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Variables index the context size.
type Var :: Ctx -> Type
type role Var nominal

#ifdef SAFE

data Var n where
    VZ :: Var (S n)
    VS :: Var n -> Var (S n)

indexVar :: Var n -> Int
indexVar = go 0 where
    go :: Int -> Var j -> Int
    go !acc VZ = acc
    go acc (VS n) = go (acc + 1) n

-- | Derive anything from variable in empty scope.
--
-- Note: don't use @EmptyCase@ as it doesn't work for unsafe representation.
absurdVar :: Var EmptyCtx -> a
absurdVar x = case x of {}

#else

-- Vars which are just 'Int's.

newtype Var j = UnsafeVar { indexVar :: Int }

-- | Derive anything from variable in empty scope.
--
-- Note: don't use @EmptyCase@ as it doesn't work for unsafe representation.
absurdVar :: Var EmptyCtx -> a
absurdVar x = x `seq` error "absurd: Var Z"

-- We need a GADT to implement pattern synonyms.
type Var' :: Nat -> Type
type role Var' nominal
data Var' n where
    VZ' :: Var' (S n)
    VS' :: Var n -> Var' (S n)

upVar :: Var n -> Var' n
upVar (UnsafeVar 0) = unsafeCoerce VZ'
upVar (UnsafeVar n) = unsafeCoerce (VS' (UnsafeVar (n - 1)))

pattern VZ :: () => (m ~ S n) => Var n
pattern VZ <- (upVar -> VZ') where
    VZ = UnsafeVar 0

pattern VS :: () => (m ~ S n) => Var n -> Var m
pattern VS n <- (upVar -> VS' n) where
    VS n = UnsafeVar (indexVar n + 1)

{-# COMPLETE VZ, VS #-}

#endif

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

deriving instance Eq (Var n)
deriving instance Ord (Var n)

instance Show (Var j) where
    showsPrec d = showsPrec d . indexVar

-- | Case on 'Var'. (compare to 'maybe').
unvar :: a -> (Var n -> a) -> Var (S n) -> a
unvar z _ VZ     = z
unvar _ s (VS x) = s x

-- | Is variable unused?
unusedVar :: Var (S n) -> Maybe (Var n)
unusedVar (VS x) = Just x
unusedVar _      = Nothing

-- | Are two variables unused?
unusedVar2 :: Var (S (S n)) -> Maybe (Var n)
unusedVar2 (VS (VS x)) = Just x
unusedVar2 _           = Nothing

-- | Are three variables unused?
unusedVar3 :: Var (S (S (S n))) -> Maybe (Var n)
unusedVar3 (VS (VS (VS x))) = Just x
unusedVar3 _                = Nothing
