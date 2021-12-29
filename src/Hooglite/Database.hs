module Hooglite.Database (
    Database,
    Entry (..),
    apiToDatabase,
    query,
) where

import Control.Monad.EitherK          (EitherKT, runEitherKT)
import Control.Monad.Trans            (lift)
import Control.Unification            (UTerm (..), applyBindings, freeVar, unify)
import Control.Unification.IntVar     (IntBindingT, IntVar, evalIntBindingT)
import Control.Unification.Types      (UFailure)
import Data.Char                      (toLower)
import Data.Either                    (isRight)
import Data.Functor.Identity          (Identity (..))
import Data.List                      (isInfixOf)
import Distribution.ModuleName        (ModuleName)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version     (Version)

import qualified Data.Map.Strict     as Map
import qualified Data.Text.Short     as ST
import qualified Distribution.Pretty as C

import Hooglite.Declaration
import Hooglite.Haddock
import Hooglite.MonoPoly
import Hooglite.MonoPoly.Name
import Hooglite.MonoPoly.Pretty
import Hooglite.MonoPoly.Var
import Hooglite.Query
import Hooglite.Ty

-- | Database for query performing.
data Database = DB
    { _dbEntries :: [Entry]
    }

instance Semigroup Database where
    DB xs <> DB ys = DB (xs <> ys)

instance Monoid Database where
    mempty = DB []

entriesToDB :: [Entry] -> Database
entriesToDB = DB

apiToDatabase :: API -> Database
apiToDatabase (API pn ver modules) = entriesToDB
    [ Entry pn ver mn name decl
    | (mn, decls) <- Map.toList modules
    , (name, decl) <- Map.toList decls
    ]

data Entry = Entry !PackageName !Version !ModuleName !Name !Declaration
  deriving Show

instance Pretty Entry where
    ppr (Entry pn ver mn name decl) =
        C.pretty pn <+> C.pretty ver <+> C.pretty mn <+> ppr name <+> ppr decl

query :: Database -> Query -> [Entry]
query (DB _entries) QueryInvalid  =
    []
query (DB entries) (QueryName n) =
    [ e
    | e@(Entry _pn _ver _mn name _decl) <- entries
    , let n' = ST.toString (unName name)
    , map toLower n `isInfixOf` map toLower n'
    ]
query (DB entries) (QueryType qty _) =
    [ e
    | e@(Entry _pn _ver _mn _name decl) <- entries
    , case decl of
        SigD (Just ty) _ -> subsumesTy qty ty
        ConD (Just ty) _ -> subsumesTy qty ty
        _                -> False
    ]

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

type Unify = EitherKT (UFailure (MonoF Z Name) IntVar) (IntBindingT (MonoF Z Name) Identity)

runUnify :: Unify a -> Either (UFailure (MonoF Z Name) IntVar) a
runUnify m = runIdentity (evalIntBindingT (runEitherKT m))

subsumesTy :: Ty -> Ty -> Bool
subsumesTy a b = isRight $ runUnify $ do
    a' <- unwrap  (Left <$> a)
    b' <- unwrap' (Left <$> b)

    ab <- unify (unroll a') (unroll b')
    _ab <- applyBindings ab
    return ()

unwrap :: Poly n (Either f IntVar) -> Unify (Mono n (Either f IntVar) )
unwrap (Mono a)    = return a
unwrap (Poly _n a) = do
    x <- lift freeVar
    unwrap (instantiate (Free (Right x)) a)

unwrap' :: Poly n (Either Name v) -> Unify (Mono n (Either Name v))
unwrap' (Mono a)           = return a
unwrap' (Poly (IName n) a) = do
    unwrap' (instantiate (Free (Left n)) a)

unroll :: Mono n (Either f IntVar) -> UTerm (MonoF n f) IntVar
unroll (Var x)          = UTerm (VarF x)
unroll (Free (Left f))  = UTerm (FreeF f)
unroll (Free (Right v)) = UVar v
unroll (App a b)        = UTerm (AppF (unroll a) (unroll b))
unroll (Arr a b)        = UTerm (ArrF (unroll a) (unroll b))
