{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Hooglite.Haddock (
    API (..),
    apiPackageId,
    parseHoogleFile,
) where

import Data.Bifunctor                 (first)
import Data.Char                      (isAlpha, isSpace)
import Data.Either                    (partitionEithers)
import Data.List                      (dropWhileEnd, isPrefixOf, stripPrefix)
import Data.Map                       (Map)
import Data.String                    (fromString)
import Distribution.ModuleName        (ModuleName)
import Distribution.Parsec            (eitherParsec)
import Distribution.Types.PackageId   (PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version     (Version)

import GHC.Hs.Decls     (HsDataDefn (..), HsDecl (..), TyClDecl (..))
import GHC.Types.SrcLoc (GenLocated (L))

import Language.Haskell.GhclibParserEx.GHC.Parser (parseDeclaration)
import Language.Haskell.Syntax.Decls (DataDefnCons (..))

import qualified Data.Map.Strict as Map

import Hooglite.Declaration
import Hooglite.GHC.Utils
import Hooglite.MonoPoly.Name
import Hooglite.MonoPoly.Pretty

-- | API of a package.
data API = API
    { apiPackage :: !PackageName
    , apiVersion :: !Version
    , apiModules :: !(Map ModuleName (Map Name Declaration))
    }
  deriving Show

apiPackageId :: API -> PackageIdentifier
apiPackageId (API pn ver _) = PackageIdentifier pn ver

parseHoogleFile :: String -> Either String API
parseHoogleFile input = do
    ls <- parseLines input
    case ls of
        LPackage pkg : LVersion ver : LModule mn : ls' -> return API
            { apiPackage = pkg
            , apiVersion = ver
            , apiModules = parseModules mn ls'
            }
        _ -> Left "api file doesn't start with package and version information"

parseModules :: ModuleName -> [Line] -> Map ModuleName (Map Name Declaration)
parseModules = aux Map.empty Map.empty where
    aux :: Map ModuleName (Map Name Declaration)
        -> Map Name Declaration
        -> ModuleName
        -> [Line]
        -> Map ModuleName (Map Name Declaration)
    aux !res !_ !_  []                     = res
    aux !res !m !mn (LPackage _ : ls)      = aux res m mn ls
    aux !res !m !mn (LVersion _ : ls)      = aux res m mn ls
    aux !res !m !mn (LModule mn' : ls)     = aux (Map.insert mn m res) Map.empty mn' ls
    aux !res !m !mn (LDecl name decl : ls) = aux res (Map.insert name decl m) mn ls

parseLines :: String -> Either String [Line]
parseLines input
    | strict    = fmap concat $ sequence ls
    | otherwise = Right $ concat $ snd $ partitionEithers ls
  where
    ls :: [Either String [Line]]
    ls = map (parseLine . cleanUpLine) (lines input)

    strict = False

-- | An entry in the Hoogle DB
data Line
    = LPackage PackageName
    | LVersion Version
    | LModule ModuleName
    | LDecl Name Declaration
  deriving Show

instance Pretty Line where
    ppr (LDecl n decl) = ppr n <+> ppr decl
    ppr d = fromString (show d)

singleton :: a -> [a]
singleton x = [x]

cleanUpLine :: String -> String
cleanUpLine = dropWhile isSpace . dropWhileEnd (\c -> c == ';' || isSpace c)

parseLine :: String -> Either String [Line]
parseLine ('-' : '-' : _)                       = return []
parseLine line | all isSpace line               = return []
parseLine line@(stripPrefix "@package " -> Just rest) = reportLine line $
    singleton . LPackage <$> eitherParsec (dropWhile isSpace rest)
parseLine line@(stripPrefix "@version " -> Just rest) = reportLine line $
    singleton . LVersion <$> eitherParsec (dropWhile isSpace rest)
parseLine line@(stripPrefix "module " -> Just rest) = reportLine line $
    singleton . LModule <$> eitherParsec (dropWhile isSpace rest)
parseLine "}"  = return []
parseLine line = reportLine line $ parseItem (fixLine line)

reportLine :: String -> Either String a -> Either String a
reportLine line = first (\err -> line ++ "\n" ++ err)

parseItem :: String -> Either String [Line]
parseItem str = first unlines $
    parseDefault `orElse`
    parseConstructor `orElse`
    -- parseNewtype `orElse`
    parseAssociateTF
  where
    parseDefault = do
        decl <- parse parseDeclaration str
        first singleton $ toDeclaration decl LDecl

    parseConstructor
        | Right (L _ (TyClD _ (DataDecl { tcdDataDefn = HsDataDefn { dd_cons = DataTypeCons False [L _ d]}}))) <- parse parseDeclaration $ "data Data where " ++ str
        = first singleton $ conToDeclaration d LDecl

        | otherwise
        = Left ["Not a constructor"]

    parseAssociateTF
        | Just rest <- stripPrefix "type " str
        , Right (decl@(L _ TyClD {})) <- parse parseDeclaration $ "type family " ++ rest
        = first singleton $ toDeclaration decl LDecl

        | otherwise
        = Left ["Not an associated type family"]

    orElse :: Either [a] b -> Either [a] b -> Either [a] b
    orElse r@Right {} _           = r
    orElse   _        l@Right {}  = l
    orElse (Left err) (Left err') = Left (err ++ err')

-- | Fix lines prior parsing, to make them look like normal Haskell.
fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x)  = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x)  = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlapping] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x)        = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x)                    = "( # ) " ++ x
fixLine ('[':x:xs)
    | isAlpha x || x `elem` ("_(" :: String)
    , (a,']':b) <- break (== ']') xs                      = x : a ++ b
fixLine ('[':':':xs)
    | (a,']':b)
    <- break (== ']') xs                                  = "(:" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x                       = fst $ breakOn " where " x
fixLine x = x

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn  needle haystack | needle `isPrefixOf` haystack = ([], haystack)
breakOn _needle []                                      = ([], [])
breakOn  needle (x:xs)                                  = first (x:) $ breakOn needle xs
