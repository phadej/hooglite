module Hooglite.Test.Query (
    queryTests,
) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseInfo, (@=?))

import Hooglite.MonoPoly.Pretty
import Hooglite.Query

queryTests :: TestTree
queryTests = testGroup "query"
    [ queryTest "a -> b" "type: forall a b. a -> b"
    , queryTest ">>>"    "name: >>>"
    ]
  where
    queryTest :: String -> String -> TestTree
    queryTest str expected = testCaseInfo str $ do
        let q = parseQuery str
        expected @=? pretty q
        return expected
