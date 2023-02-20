module Hooglite.Ty where

import Control.Monad    (join)
import Data.Char        (isLower)
import Data.List        (nub)
import Data.Maybe       (mapMaybe)
import Data.String      (fromString)
import GHC.Hs.Extension (GhcPs)
import GHC.Types.SrcLoc (GenLocated (L))
import DeBruijn

import qualified Data.Text.Short as ST
import qualified GHC.Hs.Type     as GHC

import Hooglite.GHC.Utils
import Hooglite.MonoPoly
import Hooglite.MonoPoly.Name


-------------------------------------------------------------------------------
-- Our representation for types
-------------------------------------------------------------------------------

type Ty = Poly Name EmptyCtx

genType :: Ty -> Ty
genType ty = foldr forall_ ty xs where
    xs :: [Name]
    xs = nub $ foldrPoly (\x acc -> if nameLooksLikeTyVar x then x : acc else acc) [] ty

nameLooksLikeTyVar :: Name -> Bool
nameLooksLikeTyVar (Name n) = case ST.uncons n of
    Just (c, _) -> isLower c
    _           -> False

arr_ :: Ty -> Ty -> Maybe Ty
arr_  (Mono a) (Mono b) = Just (Mono (Arr a b))
arr_  _        _        = Nothing

app_ :: Ty -> Ty -> Maybe Ty
app_  (Mono a) (Mono b) = Just (Mono (App a b))
app_  _        _        = Nothing

apps_ :: Ty -> [Ty] -> Maybe Ty
apps_ a []     = Just a
apps_ a (b:bs) = app_ a b >>= (`apps_` bs)

convType :: GHC.LHsType GhcPs -> Maybe Ty
convType = go where
    -- look at
    -- https://hackage.haskell.org/package/ghc-lib-parser-9.0.2.20211226/docs/GHC-Hs-Type.html#t:HsType
    go :: GHC.LHsType GhcPs -> Maybe Ty
    go (L _ (GHC.HsParTy _ a))         = go a
    go (L _ (GHC.HsFunTy _ _ a b))     = join $ arr_ <$> go a <*> go b
    go (L _ (GHC.HsAppTy _ a b))       = join $ app_ <$> go a <*> go b
    go (L _ (GHC.HsAppKindTy _ a b))   = join $ app_ <$> go a <*> go b
    go (L _ (GHC.HsStarTy _ _))        = Just "*"
    go (L _ (GHC.HsKindSig _ a _))     = go a
    go (L _ (GHC.HsTyVar _ _ (L _ n))) = Just (Mono (Free (toName n)))
    go (L _ (GHC.HsQualTy _ _ b))      = go b -- we forget about constraints.
    go (L _ (GHC.HsForAllTy _ xs y))   = forallTeles xs <$> go y
    go (L _ (GHC.HsListTy _ a))        = join $ app_ "List" <$> go a
    go (L _ (GHC.HsTupleTy _ _ xs))    = apps_ (fromString (tupleName (length xs))) (mapMaybe go xs)
    go (L _ ty)                        = Just (fromString (fakeShowPpr ty))

    tupleName :: Int -> String
    tupleName 0 = "Unit"
    tupleName 1 = "Solo"
    tupleName n = "Tuple" ++ show n

    forallTeles :: GHC.HsForAllTelescope GhcPs -> Ty -> Ty
    forallTeles (GHC.HsForAllVis _ xs)   y = foldr forallTeles' y xs
    forallTeles (GHC.HsForAllInvis _ xs) y = foldr forallTeles' y xs

    forallTeles' :: GHC.LHsTyVarBndr flag GhcPs -> Ty -> Ty
    forallTeles' (L _ (GHC.UserTyVar _ _ (L _ n))) b       = forall_ n' b where n' = toName n
    forallTeles' (L _ (GHC.KindedTyVar _ _ (L _ n) _ki)) b = forall_ n' b where n' = toName n
