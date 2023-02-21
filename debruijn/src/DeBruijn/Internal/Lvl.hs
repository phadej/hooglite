-- | de Bruijn levels for well-scoped terms.
module DeBruijn.Internal.Lvl (
    -- * Levels
    Lvl (UnsafeLvl),
    lvlToIdx,
    sinkLvl,
    lvlZ,
    Sinkable (..),
    sink,
    mapSink,
) where

import Data.Kind    (Constraint, Type)
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

import DeBruijn.Ctx
import DeBruijn.Internal.Idx
import DeBruijn.Internal.Size

-- | de Bruijn levels.
type Lvl :: Ctx -> Type
type role Lvl nominal
newtype Lvl ctx = UnsafeLvl { _unLvl :: Int }

-- | Convert level to index.
lvlToIdx :: Size ctx -> Lvl ctx -> Idx ctx
lvlToIdx (UnsafeSize ctx) (UnsafeLvl lvl) = UnsafeIdx (ctx - lvl - 1)

lvlZ :: Size ctx -> Lvl (S ctx)
lvlZ (UnsafeSize s) = UnsafeLvl s

-- | Sink 'Lvl' into a larger context.
sinkLvl :: Lvl n -> Lvl (S n)
sinkLvl = coerce

type Sinkable :: (Ctx -> Type) -> Constraint
class Sinkable t where
    mapLvl :: (Lvl ctx -> Lvl ctx') -> t ctx -> t ctx'

instance Sinkable Lvl where mapLvl = id

sink :: Sinkable t => t ctx -> t (S ctx)
sink = unsafeCoerce

mapSink :: (Functor f, Sinkable t) => f (t ctx) -> f (t (S ctx))
mapSink = unsafeCoerce
