module Hooglite.GHC.Utils where

import Data.Data            (Data)
import Data.List            (foldl')
import GHC.Driver.Ppr       (showPpr)
import GHC.Driver.Session   (DynFlags, defaultDynFlags, xopt_set)
import GHC.Parser.Lexer     (PState (errors), ParseResult (..))
import GHC.Types.Error      (NoDiagnosticOpts (..), getMessages)
import GHC.Utils.Error      (pprMsgEnvelopeBagWithLoc)
import GHC.Utils.Outputable (Outputable)

import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import qualified GHC.Hs.Dump
import qualified GHC.LanguageExtensions.Type as LangExt

-------------------------------------------------------------------------------
-- General helpers
-------------------------------------------------------------------------------

fakeDynFlags :: DynFlags
fakeDynFlags = defaultDynFlags fakeSettings

fakeShowPpr :: Outputable a => a -> String
fakeShowPpr = showPpr fakeDynFlags

showAstData :: Data a => a -> String
showAstData = fakeShowPpr . GHC.Hs.Dump.showAstData GHC.Hs.Dump.BlankSrcSpan GHC.Hs.Dump.BlankEpAnnotations

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
    PFailed pstate -> do
        let es = errors pstate
        Left $ map fakeShowPpr $ pprMsgEnvelopeBagWithLoc NoDiagnosticOpts $ getMessages es
