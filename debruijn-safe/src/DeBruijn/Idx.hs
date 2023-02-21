-- | Idxiables for well-scoped terms.
module DeBruijn.Idx (
    -- * Idxiables
    Idx (IZ,IS),
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

-- | Idxiables index the context size.
type Idx :: Ctx -> Type
type role Idx nominal

data Idx n where
    IZ :: Idx (S n)
    IS :: Idx n -> Idx (S n)

indexIdx :: Idx n -> Int
indexIdx = go 0 where
    go :: Int -> Idx j -> Int
    go !acc IZ = acc
    go acc (IS n) = go (acc + 1) n

-- | Derive anything from Idxiable in empty scope.
--
-- Note: don't use @EmptyCase@ as it doesn't work for unsafe representation.
absurdIdx :: Idx EmptyCtx -> a
absurdIdx x = case x of {}

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
