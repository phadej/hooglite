{-# LANGUAGE OverloadedStrings #-}
module Hooglite.Test.Haddock (
    haddockTests,
) where

import Distribution.PackageDescription (PackageName)
import Distribution.Pretty             (prettyShow)
import Distribution.Version            (Version, mkVersion)
import Test.Tasty                      (TestTree, testGroup)
import Test.Tasty.HUnit                (assertFailure, testCaseInfo, (@=?))

import Hooglite.Haddock

haddockTests :: TestTree
haddockTests = testGroup "haddock"
    [ haddockTest "base.txt"  "base"  (mkVersion [4,16,0,0]) 220
    , haddockTest "extra.txt" "extra" (mkVersion [1,7,10]) 19
    ]
  where
    haddockTest :: String -> PackageName -> Version -> Int -> TestTree
    haddockTest name pn ver ms = testCaseInfo name $ do
        contents <- readFile $ "test-data/" ++ name
        API pn' ver' ms' <- either assertFailure return $ parseHoogleFile contents
        pn @=? pn'
        ver @=? ver'
        ms @=? length ms'
        return $ prettyShow pn' ++ " " ++ prettyShow ver'
