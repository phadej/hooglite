{-# LANGUAGE OverloadedStrings #-}
module Hooglite.Declaration where

import Control.Monad    (join)
import GHC.Hs.Extension (GhcPs)
import GHC.Types.SrcLoc (GenLocated (L))

import qualified GHC.Hs.Binds          as GHC
import qualified GHC.Hs.Decls          as GHC
import qualified GHC.Hs.Type           as GHC

import Hooglite.GHC.Utils
import Hooglite.MonoPoly.Name
import Hooglite.MonoPoly.Pretty
import Hooglite.Ty

data Declaration
    = DataD           String
    | ClassD          String
    | SynD            String
    | FamD            String
    | ConD (Maybe Ty) String
    | SigD (Maybe Ty) String
  deriving Show

instance Pretty Declaration where
    ppr DataD {}     = "data"
    ppr ClassD {}    = "class"
    ppr SynD {}      = "type"
    ppr FamD {}      = "type family" -- TODO: maybe data family
    ppr (ConD ty _ ) = "constructor ::" <+> ppr ty
    ppr (SigD ty _ ) = "signature ::" <+> ppr ty

-- | How GHC's pretty printer thinks about this declaration.
declarationSrc :: Declaration -> String
declarationSrc (DataD src)  = src
declarationSrc (ClassD src) = src
declarationSrc (SynD src)   = src
declarationSrc (FamD src)   = src
declarationSrc (ConD _ src) = src
declarationSrc (SigD _ src) = src

toDeclaration :: GHC.LHsDecl GhcPs -> (Name -> Declaration -> r) -> Either String [r]
toDeclaration (L _ (GHC.TyClD _ tycld)) mk = tyclToDeclaration tycld mk
toDeclaration (L _ (GHC.SigD _ sigd))   mk = sigToDeclaration sigd mk
toDeclaration (L _ decl)                _  = Left $ "unimplemented" ++ showAstData decl

tyclToDeclaration :: GHC.TyClDecl GhcPs -> (Name -> Declaration -> r) -> Either String [r]
tyclToDeclaration d@GHC.DataDecl  { GHC.tcdLName = L _ name } mk = Right [mk (toName name) $ DataD  $ fakeShowPpr d ]
tyclToDeclaration d@GHC.SynDecl   { GHC.tcdLName = L _ name } mk = Right [mk (toName name) $ SynD   $ fakeShowPpr d ]
tyclToDeclaration d@GHC.ClassDecl { GHC.tcdLName = L _ name } mk = Right [mk (toName name) $ ClassD $ fakeShowPpr d ]
tyclToDeclaration (GHC.FamDecl _ familyDecl)                mk = famToDeclaration familyDecl mk

famToDeclaration :: GHC.FamilyDecl GhcPs -> (Name -> Declaration -> r) -> Either String [r]
famToDeclaration d@GHC.FamilyDecl { GHC.fdLName = L _ name } mk = Right [mk (toName name) $ FamD $ fakeShowPpr d ]

sigToDeclaration :: GHC.Sig GhcPs -> (Name -> Declaration -> r) -> Either String [r]
sigToDeclaration (GHC.TypeSig x names ty) mk = Right
    [ mk (toName name) $ SigD (fmap genType $ convType ty') $ fakeShowPpr $ GHC.TypeSig x [L l name] ty
    | L l name <- names
    , let ty' = GHC.hsib_body $ GHC.hswc_body ty
    ]
sigToDeclaration sig _ = Left $ "sigToDeclaration " ++ showAstData sig

conToDeclaration :: GHC.ConDecl GhcPs -> (Name -> Declaration -> r) -> Either String [r]
conToDeclaration d@GHC.ConDeclGADT { GHC.con_names = names, GHC.con_args = details, GHC.con_res_ty = ty } mk = Right
    [ mk (toName name) $ ConD (fmap genType $ join $ apps_ <$> convType ty <*> details') (fakeShowPpr (d { GHC.con_names = [L l name] } ))
    | L l name <- names
    ]
  where
    details' :: Maybe [Ty]
    details' = sequence
        [ convType arg
        | GHC.HsScaled _ arg <- GHC.hsConDeclArgTys details
        ]
conToDeclaration d@GHC.ConDeclH98 {} _mk = Left $ "Haskell98 data decl" ++ showAstData d
