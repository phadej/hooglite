{-# LANGUAGE OverloadedStrings #-}
module Hooglite.MonoPoly.Pretty (
    Pretty (..),
    pretty,
    (<+>),
    ppParen,
) where

import Data.Void        (Void, absurd)
import Text.PrettyPrint ((<+>))
import DeBruijn         (Var, indexVar)

import qualified Data.Text.Short  as ST
import qualified Text.PrettyPrint as PP

import Hooglite.MonoPoly.Name

-------------------------------------------------------------------------------
-- Pretty class
-------------------------------------------------------------------------------

pretty :: Pretty a => a -> String
pretty = show . ppr

class Pretty a where
    ppr :: a -> PP.Doc

instance a ~ Char => Pretty [a] where
    ppr = PP.text

instance Pretty (Var n) where
    ppr x = "$" <> PP.int (indexVar x)

instance Pretty Void where
    ppr = absurd

instance Pretty Int where
    ppr i = "?" <> PP.int i

instance Pretty Name where
    ppr (Name t) = PP.text (ST.toString t)

instance Pretty () where
    ppr _ = "_"

instance Pretty a => Pretty (Maybe a) where
    ppr = maybe "?" ppr

ppParen :: Bool -> NameM PP.Doc -> NameM PP.Doc
ppParen False = id
ppParen True  = fmap PP.parens
