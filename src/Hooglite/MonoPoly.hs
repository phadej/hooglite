module Hooglite.MonoPoly where

import Control.Unification (Unifiable (..))
import GHC.Generics        (Generic1)
import Data.String         (IsString (..))
import DeBruijn            (Var (VZ), Ctx, EmptyCtx, S, unvar, Renamable (..), weaken, keepRen, Env (..), lookupEnv)
import Data.Kind           (Type)

import qualified Text.PrettyPrint as PP

import Hooglite.MonoPoly.Name
import Hooglite.MonoPoly.Pretty

-------------------------------------------------------------------------------
-- Mono
-------------------------------------------------------------------------------

-- | Mono-types.
type Mono :: Type -> Ctx -> Type
data Mono a n
    = Var (Var n)
    | Free a
    | Arr (Mono a n) (Mono a n)
    | App (Mono a n) (Mono a n)
  deriving (Eq, Show)

bindMono :: Mono a n -> (a -> Mono b n) -> Mono b n
bindMono (Var x)   _ = Var x
bindMono (Free x)  k = k x
bindMono (Arr a b) k = Arr (bindMono a k) (bindMono b k)
bindMono (App f a) k = App (bindMono f k) (bindMono a k)

substMono :: (Var n -> Mono a m) -> Mono a n -> Mono a m
substMono s (Var x)   = s x
substMono _ (Free x)  = Free x
substMono s (Arr a b) = Arr (substMono s a) (substMono s b)
substMono s (App f a) = App (substMono s f) (substMono s a)

instance Renamable (Mono a) where
    rename r m = substMono (Var . rename r) m

instance IsString a => IsString (Mono a n) where
    fromString = Free . fromString

foldrMono :: (a -> b -> b) -> b -> Mono a ctx -> b
foldrMono _ z (Var _) = z
foldrMono f z (Free x) = f x z
foldrMono f z (App a b) = foldrMono f (foldrMono f z b) a 
foldrMono f z (Arr a b) = foldrMono f (foldrMono f z b) a

mapMono :: (a -> b) -> Mono a ctx -> Mono b ctx
mapMono _ (Var x) = Var x
mapMono f (Free x) = Free (f x)
mapMono f (Arr a b) = Arr (mapMono f a) (mapMono f b)
mapMono f (App a b) = App (mapMono f a) (mapMono f b)

-------------------------------------------------------------------------------
-- Poly
-------------------------------------------------------------------------------

type Poly :: Type -> Ctx -> Type
data Poly a n
    = Mono (Mono a n)             -- ^ monotypes
    | Poly IName (Poly a (S n))   -- ^ forall.
  deriving (Eq, Show)

substPoly :: (Var n -> Mono a m) -> Poly a n -> Poly a m
substPoly s (Mono a)   = Mono (substMono s a)
substPoly s (Poly n a) = Poly n (substPoly (unvar (Var VZ) (weaken . s)) a)

infixl 4 >>==
(>>==) :: Poly a n -> (a -> Mono b n) -> Poly b n
Mono a   >>== k = Mono (bindMono a k)
Poly n a >>== k = Poly n (a >>== weaken . k)

instance Renamable (Poly a) where
    rename r (Mono a)   = Mono (rename r a)
    rename r (Poly n a) = Poly n (rename (keepRen r) a)

forall_ :: Name -> Poly Name n -> Poly Name n
forall_ n p = Poly (IName n) $ weaken p >>== \n' ->
    if n == n'
    then Var VZ
    else Free n'

instantiate :: Mono a n -> Poly a (S n) -> Poly a n
instantiate x = substPoly (unvar x Var)

instance IsString a => IsString (Poly a n) where
    fromString = Mono . fromString

foldrPoly :: (a -> b -> b) -> b -> Poly a ctx -> b
foldrPoly f z (Mono x)   = foldrMono f z x
foldrPoly f z (Poly _ x) = foldrPoly f z x

mapPoly :: (a -> b) -> Poly a ctx -> Poly b ctx
mapPoly f (Mono x)   = Mono (mapMono f x)
mapPoly f (Poly n x) = Poly n (mapPoly f x)

-------------------------------------------------------------------------------
-- MonoF
-------------------------------------------------------------------------------

-- | Base-functor of 'Mono'.
type MonoF :: Type -> Ctx -> Type -> Type
data MonoF a n b
    = VarF (Var n)
    | FreeF a
    | ArrF b b
    | AppF b b
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic1)

instance Eq a => Unifiable (MonoF a n) where

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

instance (ToName a, EmptyCtx ~ ctx) => Pretty (Mono a ctx) where
    ppr t = runNameM $ do
        usedNames (foldrMono (:) [] t)
        pprMono EmptyEnv 0 t 

pprMono :: ToName a => Env n Name -> Int -> Mono a n -> NameM PP.Doc
pprMono env _ (Var x)   = return $ ppr $ lookupEnv x env
pprMono _   _ (Free x)  = return $ ppr $ toName x
pprMono env d (App f t) = ppParen (d >= 11) $ do
    f' <- pprMono env 10 f
    t' <- pprMono env 11 t
    return $ f' <+> t'
pprMono env d (Arr a b)  = ppParen (d >= 2) $ do
    a' <- pprMono env 2 a
    b' <- pprMono env 1 b
    return $ a' <+> PP.text "->" <+> b'

instance (ToName a, EmptyCtx ~ ctx) => Pretty (Poly a ctx) where
    ppr t = runNameM $ do
        usedNames (foldrPoly (:) [] t)
        pprPoly EmptyEnv 0 t 

pprPoly :: forall a ctx. ToName a => Env ctx Name -> Int -> Poly a ctx -> NameM PP.Doc
pprPoly env0 d = go env0 [] where
    go :: Env ctx' Name -> [PP.Doc] -> Poly a ctx' -> NameM PP.Doc
    go env [] (Mono a) = pprMono env d a
    go env ns (Mono a) = do
        a' <- pprMono env 0 a
        return $ "forall" <+> PP.hsep (reverse ns) PP.<> "." <+> a'

    go env ns (Poly (IName n) a) = do
        n' <- freshName n
        go (n' ::: env) (ppr n' : ns) a
