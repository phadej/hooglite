module Hooglite.GHC.Utils where

import Data.Data            (Data)
import Data.List            (foldl')
import GHC.Driver.Session   (DynFlags, defaultDynFlags, xopt_set)
import GHC.Parser.Lexer     (ParseResult (..), getMessages)
import GHC.Utils.Error      (pprErrMsgBagWithLoc)
import GHC.Utils.Outputable (Outputable, showPpr)

import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeLlvmConfig, fakeSettings)

import qualified GHC.Hs.Dump
import qualified GHC.LanguageExtensions.Type as LangExt

-------------------------------------------------------------------------------
-- General helpers
-------------------------------------------------------------------------------

fakeDynFlags :: DynFlags
fakeDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

fakeShowPpr :: Outputable a => a -> String
fakeShowPpr = showPpr fakeDynFlags

showAstData :: Data a => a -> String
showAstData = fakeShowPpr . GHC.Hs.Dump.showAstData GHC.Hs.Dump.BlankSrcSpan

-------------------------------------------------------------------------------
-- More dynflags
-------------------------------------------------------------------------------

dynFlags :: DynFlags
dynFlags = foldl' xopt_set fakeDynFlags
    [ LangExt.ConstraintKinds
    , LangExt.DataKinds
    , LangExt.EmptyDataDecls
    , LangExt.ExplicitForAll
    , LangExt.FlexibleContexts
    , LangExt.FunctionalDependencies
    , LangExt.GADTs
    , LangExt.ImplicitParams
    , LangExt.KindSignatures
    , LangExt.MagicHash
    , LangExt.MultiParamTypeClasses
    , LangExt.ParallelArrays
    , LangExt.PatternSynonyms
    , LangExt.PolyKinds
    , LangExt.TypeFamilies
    , LangExt.TypeOperators
    , LangExt.UnboxedTuples
    , LangExt.UnicodeSyntax
    ]

-------------------------------------------------------------------------------
-- Parsing related
-------------------------------------------------------------------------------

parse :: (String -> DynFlags -> ParseResult a) -> String -> Either [String] a
parse p s = case p s dynFlags of
    POk _ x       -> Right x
    PFailed state -> do
        let (_warns, errors) = getMessages state dynFlags
        Left $ map fakeShowPpr $ pprErrMsgBagWithLoc errors
