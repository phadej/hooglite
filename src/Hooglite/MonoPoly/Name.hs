{-# LANGUAGE PolyKinds #-}
module Hooglite.MonoPoly.Name (
    -- * Name
    Name (..),
    unName,
    ToName (..),
    -- * Irrelevant name
    IName (..),
    -- * Fresh name generation monad
    NameM (..),
    runNameM,
    usedNames,
    freshName,
) where

import Control.Monad.State   (State, evalState, get, modify', put)
import Data.ByteString.Short (ShortByteString)
import Data.Char             (isDigit)
import Data.Maybe            (fromMaybe)
import Data.Set              (Set)
import Data.String           (IsString (..))
import Data.Text.Short       (ShortText)
import Text.Read             (readMaybe)

import qualified Data.Set                  as Set
import qualified Data.Text.Short           as ST
import qualified GHC.Data.FastString       as FS
import qualified GHC.Types.Name.Occurrence as OccName
import qualified GHC.Types.Name.Reader     as RdrName

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

newtype Name = Name ShortText
  deriving newtype (Eq, Ord, Show)

unName :: Name -> ShortText
unName (Name n) = n

instance IsString Name where
    fromString = Name . fromString

-------------------------------------------------------------------------------
-- Conversion to Name
-------------------------------------------------------------------------------

class ToName n where
    toName :: n -> Name

instance ToName Name where
    toName = id

instance ToName FS.FastString where
    toName = Name . lenientFromSBS . FS.fs_sbs

instance ToName OccName.OccName where
    toName = toName . OccName.occNameFS

instance ToName RdrName.RdrName where
    toName = toName .  RdrName.rdrNameOcc

-------------------------------------------------------------------------------
-- Irrelevant name
-------------------------------------------------------------------------------

newtype IName = IName Name
  deriving newtype Show

instance Eq IName where
    _ == _ = True

instance IsString IName where
    fromString = IName . fromString

-------------------------------------------------------------------------------
-- Name monad
-------------------------------------------------------------------------------

newtype NameM a = NameM { unNameM :: State (Set Name) a }
  deriving newtype (Functor, Applicative, Monad)

runNameM :: NameM a -> a
runNameM (NameM m) = evalState m Set.empty

usedNames :: ToName a => [a] -> NameM ()
usedNames xs = NameM $ modify' (Set.union (Set.fromList (map toName xs)))

freshName :: Name -> NameM Name
freshName n = NameM $ do
    s <- get
    if Set.notMember n s
    then do
        put (Set.insert n s)
        return n
    else go s (fromMaybe (0 :: Int) (readMaybe (ST.toString sfx)))
  where
    (pfx, sfx) = ST.spanEnd isDigit (unName n)

    go s !idx = do
        let n' = Name (pfx <> fromString (show idx))
        if Set.notMember n' s
        then do
            put (Set.insert n s)
            return n
        else
            go s (idx + 1)

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

lenientFromSBS :: ShortByteString -> ShortText
lenientFromSBS sbs = fromMaybe
    (error "invalid UTF-8 SBS")
    (ST.fromShortByteString sbs)
