{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.Foldable            (for_)
import Data.Traversable         (for)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment       (getArgs)

import Hooglite

main :: IO ()
main = do
    args <- getArgs
    db <- fmap mconcat $ for args $ \arg -> do
        contents <- readFile arg
        api <- either fail return $ parseHoogleFile contents
        return $! apiToDatabase api

    runInputT defaultSettings $ do
        outputStrLn "welcome to hooglite"
        repl db

repl :: Database -> InputT IO ()
repl db = getInputLine "?> " >>= \case
    Nothing    -> return ()
    Just input -> do
        let q = parseQuery input
        outputStrLn $ "query: " ++ pretty q

        let result = query db q
        for_ result $ \(Entry _ _ _ _ decl) ->
            outputStrLn (declarationSrc decl)

        repl db
