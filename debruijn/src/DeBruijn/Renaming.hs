module DeBruijn.Renaming (
    -- * Renamings
    Renaming,
    mkRenaming,
    renameVar,
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

-- | Renamings are mappings of variable.
type Renaming :: Ctx -> Ctx -> Type
newtype Renaming n m = Renaming
    { renameVar :: Var n -> Var m -- ^ Apply 'Renaming' to a variable.
    }

-- | Identity renamings.
idRen :: Renaming n n
idRen = Renaming id

-- | Make a 'Renaming' from a function.
mkRenaming :: (Var n -> Var m) -> Renaming n m
mkRenaming = Renaming

-- | Lift renaming (used when going under a binder).
keepRen :: Renaming n m -> Renaming (S n) (S m)
keepRen (Renaming f) = Renaming (go f)
  where
    go :: (Var n -> Var m) -> Var (S n) -> Var (S m)
    go _ VZ     = VZ
    go g (VS x) = VS (g x)

-- | Skip binding.
skipRen :: Renaming n m -> Renaming n (S m)
skipRen (Renaming f) = Renaming (VS . f)

-- | Renaming composition.
compRen :: Renaming a b -> Renaming b c -> Renaming a c
compRen (Renaming r) (Renaming r') = Renaming (r' . r)

instance C.Category Renaming where
    id  = idRen
    (.) = flip compRen

-- | Weakening of a context.
wkRen :: Renaming n (S n)
wkRen = Renaming VS

-- | Common renaming weakening under one variable.
--
-- @
-- 'bump' = 'liftRen' 'weaken'
-- @
bumpRen :: Renaming (S n) (S (S n))
bumpRen = keepRen wkRen

-- | Swap two top variables in the context.
--
-- /Note:/ this is one case why we cannot use thinnings.
swapRen :: Renaming (S (S n)) (S (S n))
swapRen = Renaming swap' where
    swap' :: Var (S (S n)) -> Var (S (S n))
    swap' VZ      = VS VZ
    swap' (VS VZ) = VZ
    swap' v       = v

-- | Zero variables can be renamed to any number of variables.
absurdRen :: Renaming EmptyCtx m
absurdRen = Renaming absurdVar

-------------------------------------------------------------------------------
-- Renamable
-------------------------------------------------------------------------------

-- | Renamable things.
class Renamable t where
    rename :: Renaming n m -> t n -> t m

instance Renamable Proxy where
    rename _ _ = Proxy

instance Renamable Var where
    rename = renameVar

instance Renamable (Renaming n) where
    rename g f = f C.>>> g

weaken :: Renamable t => t n -> t (S n)
weaken = rename wkRen