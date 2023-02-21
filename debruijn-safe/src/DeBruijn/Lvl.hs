module DeBruijn.Lvl (
    Lvl,
    lvlToIdx,
    lvlZ,
    sinkLvl,
    Sinkable (..),
    sink,
    mapSink,
) where

import Data.Kind (Constraint, Type)

import DeBruijn.Ctx
import DeBruijn.Idx
import DeBruijn.Size

-- | de Bruijn levels.
--
-- @forall ctx'. 'Size' ctx' -> LE ctx ctx' -> 'Idx' ctx'@
type Lvl :: Ctx -> Type
type role Lvl nominal
newtype Lvl ctx = MkLvl (Idx ctx)
  deriving (Eq, Ord, Show)

lvlToIdx :: Size ctx -> Lvl ctx -> Idx ctx
lvlToIdx _ (MkLvl x) = x

sinkLvl :: Lvl ctx -> Lvl (S ctx)
sinkLvl (MkLvl i) = MkLvl (IS i)

lvlZ :: Size ctx -> Lvl (S ctx)
lvlZ _ = MkLvl IZ

type Sinkable :: (Ctx -> Type) -> Constraint
class Sinkable t where
    mapLvl :: (Lvl ctx -> Lvl ctx') -> t ctx -> t ctx'

instance Sinkable Lvl where mapLvl = id

sink :: Sinkable t => t ctx -> t (S ctx)
sink = mapLvl sinkLvl

mapSink :: (Functor f, Sinkable t) => f (t ctx) -> f (t (S ctx))
mapSink = fmap sink
