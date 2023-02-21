-- | de Bruijn levels for well-scoped terms.
module DeBruijn.Internal.Lvl (
    -- * Levels
    Lvl (UnsafeLvl),
    lvlToIdx,
    sinkLvl,
    lvlZ,
) where

import Data.Kind    (Type)
import Data.Coerce (coerce)

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
