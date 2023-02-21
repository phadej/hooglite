module DeBruijn.Internal.Size (
    Size (SZ, SS, UnsafeSize)  
) where

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

import DeBruijn.Ctx

-- | Term level witness of the size of the context.
type Size :: Ctx -> Type
type role Size nominal

newtype Size ctx = UnsafeSize { _indexSize :: Int }

-------------------------------------------------------------------------------
-- Pattern synonyms
-------------------------------------------------------------------------------

-- We need a GADT to implement pattern synonyms.
type Size' :: Ctx -> Type
type role Size' nominal
data Size' ctx where
    SZ' :: Size' EmptyCtx 
    SS' :: Size n -> Size' (S n)

upSize :: Size n -> Size' n
upSize (UnsafeSize 0) = unsafeCoerce SZ'
upSize (UnsafeSize n) = unsafeCoerce (SS' (UnsafeSize (n - 1)))

pattern SZ :: () => (m ~ EmptyCtx) => Size m
pattern SZ <- (upSize -> SZ') where
    SZ = UnsafeSize 0

pattern SS :: () => (m ~ S n) => Size n -> Size m
pattern SS n <- (upSize -> SS' n) where
    SS n = UnsafeSize (_indexSize n + 1)

{-# COMPLETE SZ, SS #-}