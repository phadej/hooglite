{-# LANGUAGE OverloadedStrings #-}
module Hooglite.Query (
    Query (..),
    parseQuery,
) where

import Data.Char (isAlphaNum)

import Language.Haskell.GhclibParserEx.GHC.Parser (parseType)

import qualified Text.PrettyPrint as PP

import Hooglite.GHC.Utils
import Hooglite.MonoPoly.Pretty
import Hooglite.Ty

-- | Query type.
data Query
    = QueryName String      -- ^ query by (part of the name)
    | QueryType Ty String   -- ^ query by type
    | QueryInvalid
  deriving (Eq, Show)

instance Pretty Query where
    ppr (QueryName n)       = "name:" <+> PP.text n
    ppr (QueryType ty _str) = "type:" <+> ppr ty
    ppr QueryInvalid        = "<invalid>"

-- | Parse query.
--
-- If query is a single word, then we query by name.
-- Otherwise we try to parse a type.
--
parseQuery :: String -> Query
parseQuery str
    | all isAlphaNum str || all isOperator str
    = QueryName str

    | Right t <- parse parseType str
    , Just ty <- genType <$> convType t
    = QueryType ty (fakeShowPpr t)

    | otherwise
    = QueryInvalid
  where
    isOperator c = c `elem` ("!#$%&*+./<=>?@" :: String)
