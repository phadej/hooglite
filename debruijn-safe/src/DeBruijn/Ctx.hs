module DeBruijn.Ctx (
    Ctx,
    EmptyCtx,
    S,
) where

import Data.Nat  (Nat (..))
import Data.Kind (Type)

-- | Context counts variables, in other words context is just a natural number.
type Ctx :: Type
type Ctx = Nat

-- | Empty context is zero.
type EmptyCtx :: Ctx
type EmptyCtx = Z

-- | And context extension is a successor.
type S :: Ctx -> Ctx
type S = 'S