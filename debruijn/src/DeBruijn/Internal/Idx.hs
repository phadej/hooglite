-- | de Bruijn indices for well-scoped terms.
module DeBruijn.Internal.Idx (
    -- * Idxiables
    Idx (IZ,IS,UnsafeIdx),
    absurdIdx,
    unIdx,
    indexIdx,
    -- ** Common patterns
    unusedIdx,
    unusedIdx2,
    unusedIdx3,
) where

import Data.Kind            (Type)
import DeBruijn.Ctx

import Unsafe.Coerce (unsafeCoerce)

-- | Idxiables index the context size.
type Idx :: Ctx -> Type
type role Idx nominal

newtype Idx j = UnsafeIdx { _indexIdx :: Int }

indexIdx :: Idx ctx -> Int
indexIdx = _indexIdx

-- | Derive anything from Idxiable in empty scope.
--
-- Note: don't use @EmptyCase@ as it doesn't work for unsafe representation.
absurdIdx :: Idx EmptyCtx -> a
absurdIdx x = x `seq` error "absurd: Idx Z"

-------------------------------------------------------------------------------
-- Pattern synonyms
-------------------------------------------------------------------------------

-- We need a GADT to implement pattern synonyms.
type Idx' :: Ctx -> Type
type role Idx' nominal
data Idx' n where
    IZ' :: Idx' (S n)
    IS' :: Idx n -> Idx' (S n)

upIdx :: Idx n -> Idx' n
upIdx (UnsafeIdx 0) = unsafeCoerce IZ'
upIdx (UnsafeIdx n) = unsafeCoerce (IS' (UnsafeIdx (n - 1)))

pattern IZ :: () => (m ~ S n) => Idx m
pattern IZ <- (upIdx -> IZ') where
    IZ = UnsafeIdx 0

pattern IS :: () => (m ~ S n) => Idx n -> Idx m
pattern IS n <- (upIdx -> IS' n) where
    IS n = UnsafeIdx (indexIdx n + 1)

{-# COMPLETE IZ, IS #-}

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

deriving instance Eq (Idx n)
deriving instance Ord (Idx n)

instance Show (Idx j) where
    showsPrec d = showsPrec d . indexIdx

-- | Case on 'Idx'. (compare to 'maybe').
unIdx :: a -> (Idx n -> a) -> Idx (S n) -> a
unIdx z _ IZ     = z
unIdx _ s (IS x) = s x

-- | Is Idxiable unused?
unusedIdx :: Idx (S n) -> Maybe (Idx n)
unusedIdx (IS x) = Just x
unusedIdx _      = Nothing

-- | Are two Idxiables unused?
unusedIdx2 :: Idx (S (S n)) -> Maybe (Idx n)
unusedIdx2 (IS (IS x)) = Just x
unusedIdx2 _           = Nothing

-- | Are three Idxiables unused?
unusedIdx3 :: Idx (S (S (S n))) -> Maybe (Idx n)
unusedIdx3 (IS (IS (IS x))) = Just x
unusedIdx3 _                = Nothing
