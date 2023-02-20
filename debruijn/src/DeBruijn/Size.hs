module DeBruijn.Size where

import Data.Kind (Type)

import DeBruijn.Ctx

-- | Term level witness of the size of the context.
type Size :: Ctx -> Type
data Size ctx where
    SZ :: Size EmptyCtx
    SS :: Size ctx -> Size (S ctx)