{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment ( getArgs )
import Text.Parsec ( runParser )
import qualified Data.Map as Map
import Tokenizer ( sample )
import Types (SymTable(SymTable, nameTable, objTable))

main :: IO ()
main = do
    (fp : args) <- getArgs
    src <- readFile fp
    print $ runParser sample (SymTable { nameTable = Map.empty, objTable = Map.empty }) fp src