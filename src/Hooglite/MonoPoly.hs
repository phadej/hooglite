module Hooglite.MonoPoly where

import Control.Monad       (ap)
import Control.Unification (Unifiable (..))
import GHC.Generics        (Generic1)
import Data.String (IsString (..))

import qualified Text.PrettyPrint as PP

import Hooglite.MonoPoly.Name
import Hooglite.MonoPoly.Pretty
import Hooglite.MonoPoly.Var

-------------------------------------------------------------------------------
-- Mono
-------------------------------------------------------------------------------

-- | Mono-types.
data Mono n a
    = Var (Var n)
    | Free a
    | Arr (Mono n a) (Mono n a)
    | App (Mono n a) (Mono n a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (Mono n) where
    pure = Free
    (<*>) = ap

instance Monad (Mono n) where
    return = pure

    Var x   >>= _ = Var x
    Free x  >>= k = k x
    Arr a b >>= k = Arr (a >>= k) (b >>= k)
    App f a >>= k = App (f >>= k) (a >>= k)

substMono :: (Var n -> Mono m a) -> Mono n a -> Mono m a
substMono s (Var x)   = s x
substMono _ (Free x)  = Free x
substMono s (Arr a b) = Arr (substMono s a) (substMono s b)
substMono s (App f a) = App (substMono s f) (substMono s a)

instance Renamable Mono where
    r <@> m = substMono (Var . renameVar r) m

instance IsString a => IsString (Mono n a) where
    fromString = Free . fromString

-------------------------------------------------------------------------------
-- Poly
-------------------------------------------------------------------------------

data Poly n a
    = Mono (Mono n a)             -- ^ monotypes
    | Poly IName (Poly (S n) a)   -- ^ forall.
  deriving (Eq, Show, Functor, Foldable, Traversable)

substPoly :: (Var n -> Mono m a) -> Poly n a -> Poly m a
substPoly s (Mono a)   = Mono (substMono s a)
substPoly s (Poly n a) = Poly n (substPoly (unvar (Var VZ) ((weaken <@>) . s)) a)

infixl 4 >>==
(>>==) :: Poly n a -> (a -> Mono n b) -> Poly n b
Mono a   >>== k = Mono (a >>= k)
Poly n a >>== k = Poly n (a >>== (weaken <@>) . k)

instance Renamable Poly where
    r <@> Mono a   = Mono (r <@> a)
    r <@> Poly n a = Poly n (liftRen r <@> a)

forall_ :: Name -> Poly n Name -> Poly n Name
forall_ n p = Poly (IName n) $ weaken <@> p >>== \n' ->
    if n == n'
    then Var VZ
    else Free n'

instantiate :: Mono n a -> Poly (S n) a -> Poly n a
instantiate x = substPoly (unvar x Var)


instance IsString a => IsString (Poly n a) where
    fromString = Mono . fromString

-------------------------------------------------------------------------------
-- MonoF
-------------------------------------------------------------------------------

-- | Base-functor of 'Mono'.
data MonoF n a b
    = VarF (Var n)
    | FreeF a
    | ArrF b b
    | AppF b b
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic1)

instance Eq a => Unifiable (MonoF n a) where


-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

instance ToName a => Pretty (Mono n a) where
    ppr t = runNameM $ do
        let t' = fmap toName t
        usedNames t'
        pprMono 0 t' 

pprMono :: Int -> Mono n Name -> NameM PP.Doc
pprMono _ (Var x)   = return $ ppr x
pprMono _ (Free x)  = return $ PP.text (pretty x)
pprMono d (App f t) = ppParen (d >= 11) $ do
    f' <- pprMono 10 f
    t' <- pprMono 11 t
    return $ f' <+> t'
pprMono d (Arr a b)  = ppParen (d >= 2) $ do
    a' <- pprMono 2 a
    b' <- pprMono 1 b
    return $ a' <+> PP.text "->" <+> b'

instance ToName a => Pretty (Poly n a) where
    ppr t = runNameM $ do
        let t' = fmap toName t
        usedNames t'
        pprPoly 0 t' 

pprPoly :: Int -> Poly n Name -> NameM PP.Doc
pprPoly d = go [] where
    go [] (Mono a) = pprMono d a
    go ns (Mono a) = do
        a' <- pprMono 0 a
        return $ "forall" <+> PP.hsep (reverse ns) PP.<> "." <+> a'

    go ns (Poly (IName n) a) = do
        n' <- freshName n
        go (ppr n' : ns) (instantiate (Free n') a)
