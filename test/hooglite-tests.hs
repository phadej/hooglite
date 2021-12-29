module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Hooglite.Test.Query
import Hooglite.Test.Haddock
import Hooglite.Test.Database

main :: IO ()
main = defaultMain $ testGroup "hooglite"
    [ queryTests
    , haddockTests
    , databaseTests
    ]
