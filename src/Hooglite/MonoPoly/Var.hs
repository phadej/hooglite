{-# LANGUAGE CPP #-}
-- | Variables for well-scoped terms.
module Hooglite.MonoPoly.Var (
    -- * Variables
    Nat (..),
    Var (VZ,VS),
    absurdVar,
    unvar,
    indexVar,
    -- ** Common patterns
    unusedVar,
    unusedVar2,
    unusedVar3,
    -- * Renamings
    Renaming,
    mkRenaming,
    renameVar,
    idRenaming,
    liftRen,
    (>>>),
    bump,
    swap,
    weaken,
    absurdRen,
    -- * Renamable things
    Renamable (..),
    Renamable0 (..),
    NoCtx (..),
) where

import Data.Bifunctor.Clown (Clown (..))
import Data.Bifunctor.Flip  (Flip (..))
import Data.Kind            (Type)
import Data.Nat             (Nat (..))

import qualified Control.Category as C

#ifdef SAFE
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Variables index the context size.
type Var :: Nat -> Type
type role Var nominal

#ifdef SAFE

data Var n where
    VZ :: Var (S n)
    VS :: Var n -> Var (S n)

indexVar :: Var n -> Int
indexVar = go 0 where
    go :: Int -> Var j -> Int
    go !acc VZ = acc
    go acc (VS n) = go (acc + 1) n

-- | Derive anything from variable in empty scope.
--
-- Note: don't use @EmptyCase@ as it doesn't work for unsafe representation.
absurdVar :: Var Z -> a
absurdVar x = case x of {}

#else

-- Vars which are just 'Int's.

newtype Var j = UnsafeVar { indexVar :: Int }

-- | Derive anything from variable in empty scope.
--
-- Note: don't use @EmptyCase@ as it doesn't work for unsafe representation.
absurdVar :: Var Z -> a
absurdVar x = x `seq` error "absurd: Var Z"

-- We need a GADT to implement pattern synonyms.
type Var' :: Nat -> Type
type role Var' nominal
data Var' n where
    VZ' :: Var' (S n)
    VS' :: Var n -> Var' (S n)

upVar :: Var n -> Var' n
upVar (UnsafeVar 0) = unsafeCoerce VZ'
upVar (UnsafeVar n) = unsafeCoerce (VS' (UnsafeVar (n - 1)))

pattern VZ :: () => (m ~ S n) => Var n
pattern VZ <- (upVar -> VZ') where
    VZ = UnsafeVar 0

pattern VS :: () => (m ~ S n) => Var n -> Var m
pattern VS n <- (upVar -> VS' n) where
    VS n = UnsafeVar (indexVar n + 1)

{-# COMPLETE VZ, VS #-}

#endif

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

deriving instance Eq (Var n)
deriving instance Ord (Var n)

instance Show (Var j) where
    showsPrec d = showsPrec d . indexVar

-- | Case on 'Var'. (compare to 'maybe').
unvar :: a -> (Var n -> a) -> Var (S n) -> a
unvar z _ VZ     = z
unvar _ s (VS x) = s x

-- | Is variable unused?
unusedVar :: Var (S n) -> Maybe (Var n)
unusedVar (VS x) = Just x
unusedVar _      = Nothing

-- | Are two variables unused?
unusedVar2 :: Var (S (S n)) -> Maybe (Var n)
unusedVar2 (VS (VS x)) = Just x
unusedVar2 _           = Nothing

-- | Are three variables unused?
unusedVar3 :: Var (S (S (S n))) -> Maybe (Var n)
unusedVar3 (VS (VS (VS x))) = Just x
unusedVar3 _                = Nothing

-------------------------------------------------------------------------------
-- Renamings
-------------------------------------------------------------------------------

-- | Renamings are mappings of variable.
type Renaming :: Nat -> Nat -> Type
newtype Renaming n m = Renaming
    { renameVar :: Var n -> Var m -- ^ Apply 'Renaming' to a variable.
    }

-- | Identity renamings.
idRenaming :: Renaming n n
idRenaming = Renaming id

-- | Make a 'Renaming' from a fuinction.
mkRenaming :: (Var n -> Var m) -> Renaming n m
mkRenaming = Renaming

-- | Lift renaming (used when going under a binder).
liftRen :: Renaming n m -> Renaming (S n) (S m)
liftRen (Renaming f) = Renaming (go f)
  where
    go :: (Var n -> Var m) -> Var (S n) -> Var (S m)
    go _ VZ     = VZ
    go g (VS x) = VS (g x)

-- we need to bind tighter then <@>
infixr 9 >>>

-- | Renaming composition.
(>>>) :: Renaming a b -> Renaming b c -> Renaming a c
Renaming r >>> Renaming r' = Renaming (r' . r)

instance C.Category Renaming where
    id  = idRenaming
    (.) = flip (>>>)

-- | Weakening of a context.
weaken :: Renaming n (S n)
weaken = Renaming VS

-- | Common renaming weakening under one variable.
--
-- @
-- 'bump' = 'liftRen' 'weaken'
-- @
bump :: Renaming (S n) (S (S n))
bump = liftRen weaken

-- | Swap two top variables in the context.
--
-- /Note:/ this is one case why we cannot use thinnings.
swap :: Renaming (S (S n)) (S (S n))
swap = Renaming swap' where
    swap' :: Var (S (S n)) -> Var (S (S n))
    swap' VZ      = VS VZ
    swap' (VS VZ) = VZ
    swap' v       = v

-- | Zero variables can be renamed to any number of variables.
absurdRen :: Renaming Z m
absurdRen = Renaming absurdVar

-------------------------------------------------------------------------------
-- Renamable
-------------------------------------------------------------------------------

-- | Renamable things.
class Renamable t where
    (<@>) :: Renaming n m -> t n a -> t m a

-- | Renamable things.
--
-- A more correct type-class.
-- However 'Renamable' is more convenient as the kinds much the term.
-- You can use 'Flip' and 'Clown' to convert between these.
class Renamable0 t where
    (<@@>) :: Renaming n m -> t n -> t m

infixl 4 <@>, <@@>

instance Renamable0 t =>  Renamable (Clown t) where
    f <@> Clown x = Clown (f <@@> x)

instance Renamable t => Renamable0 (Flip t a) where
    f <@@> Flip x = Flip (f <@> x)

-- | No context.
--
-- Used to implement 'rewrite' in terms of 'rewriteWith' etc.
type NoCtx :: Nat -> Type -> Type
data NoCtx n a = NoCtx

instance Renamable NoCtx where
    _ <@> _ = NoCtx

instance Renamable0 Var where
    r <@@> x = renameVar r x

instance Renamable0 (Renaming n) where
    f <@@> g = g >>> f
