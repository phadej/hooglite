module Hooglite (
    -- * Database
    Database,
    Entry (..),
    apiToDatabase,
    query,
    -- * Queries
    Query (..),
    parseQuery,
    -- * Parsing hoogle.txt files
    API (..),
    apiPackageId,
    parseHoogleFile,
    -- * Declarations
    Declaration (..),
    declarationSrc,
    -- * Extras
    pretty,
) where

import Hooglite.Database
import Hooglite.Declaration
import Hooglite.Haddock
import Hooglite.MonoPoly.Pretty
import Hooglite.Query
