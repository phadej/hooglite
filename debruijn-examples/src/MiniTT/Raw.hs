module MiniTT.Raw where

import Data.Kind (Type)
import Data.Text.Short (ShortText)

newtype Name = Name ShortText
  deriving (Eq, Ord, Show)

type Raw :: Type
data Raw where
    RLam :: Name -> Raw -> Raw         -- ^ function abstraction: @\\x -> t@
    RPie :: Name -> Raw -> Raw -> Raw  -- ^ pi type: @(x : T) -> S
    RTyp :: Raw                        -- ^ type: @U@
    RVar :: Name -> Raw                -- ^ variable: @x@
    RApp :: Raw -> Raw -> Raw          -- ^ function application: @f t@
    RAnn :: Raw -> Raw                 -- ^ type annotation: @t : T@
    RLet :: Name -> Raw -> Raw -> Raw  -- ^ let binding: @let x : t = s in e@ or @let x = f in e@
  deriving Show
