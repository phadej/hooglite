module DeBruijn.Renaming (
    -- * Renamings
    Renaming,
    mkRenaming,
    renameIdx,
    idRen,
    compRen,
    keepRen,
    skipRen,
    bumpRen,
    swapRen,
    wkRen,
    absurdRen,
    -- * Renamable things
    Renamable (..),
    weaken,
) where

import Data.Proxy (Proxy (..))
import Data.Kind  (Type)

import qualified Control.Category as C

import DeBruijn.Ctx
import DeBruijn.Idx

-------------------------------------------------------------------------------
-- Renamings
-------------------------------------------------------------------------------

-- | Renamings are mappings of Idxiable.
type Renaming :: Ctx -> Ctx -> Type
newtype Renaming n m = Renaming
    { renameIdx :: Idx n -> Idx m -- ^ Apply 'Renaming' to a Idxiable.
    }

-- | Identity renamings.
idRen :: Renaming n n
idRen = Renaming id

-- | Make a 'Renaming' from a function.
mkRenaming :: (Idx n -> Idx m) -> Renaming n m
mkRenaming = Renaming

-- | Lift renaming (used when going under a binder).
keepRen :: Renaming n m -> Renaming (S n) (S m)
keepRen (Renaming f) = Renaming (go f)
  where
    go :: (Idx n -> Idx m) -> Idx (S n) -> Idx (S m)
    go _ IZ     = IZ
    go g (IS x) = IS (g x)

-- | Skip binding.
skipRen :: Renaming n m -> Renaming n (S m)
skipRen (Renaming f) = Renaming (IS . f)

-- | Renaming composition.
compRen :: Renaming a b -> Renaming b c -> Renaming a c
compRen (Renaming r) (Renaming r') = Renaming (r' . r)

instance C.Category Renaming where
    id  = idRen
    (.) = flip compRen

-- | Weakening of a context.
wkRen :: Renaming n (S n)
wkRen = Renaming IS

-- | Common renaming weakening under one Idxiable.
--
-- @
-- 'bump' = 'liftRen' 'weaken'
-- @
bumpRen :: Renaming (S n) (S (S n))
bumpRen = keepRen wkRen

-- | Swap two top Idxiables in the context.
--
-- /Note:/ this is one case why we cannot use thinnings.
swapRen :: Renaming (S (S n)) (S (S n))
swapRen = Renaming swap' where
    swap' :: Idx (S (S n)) -> Idx (S (S n))
    swap' IZ      = IS IZ
    swap' (IS IZ) = IZ
    swap' v       = v

-- | Zero Idxiables can be renamed to any number of Idxiables.
absurdRen :: Renaming EmptyCtx m
absurdRen = Renaming absurdIdx

-------------------------------------------------------------------------------
-- Renamable
-------------------------------------------------------------------------------

-- | Renamable things.
class Renamable t where
    rename :: Renaming n m -> t n -> t m

instance Renamable Proxy where
    rename _ _ = Proxy

instance Renamable Idx where
    rename = renameIdx

instance Renamable (Renaming n) where
    rename g f = f C.>>> g

weaken :: Renamable t => t n -> t (S n)
weaken = rename wkRen