{-# LANGUAGE OverloadedStrings #-}
module Hooglite.Test.Database (
    databaseTests,
) where

import Test.Tasty       (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCaseInfo, (@=?))

import Hooglite.Database
import Hooglite.Haddock
import Hooglite.MonoPoly.Pretty
import Hooglite.Query

databaseTests :: TestTree
databaseTests =
    withResource (database "extra.txt") (\_ -> return ()) $ \loadDb ->
    testGroup "database-queries"
        [ queryTest loadDb "fst3" 2
        , queryTest loadDb "dropEnd" 4
        , queryTest loadDb "Int -> Double" 2
        , queryTest loadDb "Int -> a" 18
        , queryTest loadDb "(a,b,c) -> a" 2
        ]
  where
    database :: String -> IO Database
    database name = do
        contents <- readFile $ "test-data/" ++ name
        api <- either fail return $ parseHoogleFile contents
        return $! apiToDatabase api

    queryTest :: IO Database -> String -> Int -> TestTree
    queryTest loadDb q expected = testCaseInfo q $ do
        db <- loadDb
        let result = query db $ parseQuery q
        expected @=? length result
        return $ case result of
            []    -> "no results"
            _     -> unlines $ take 10 $ map pretty result
