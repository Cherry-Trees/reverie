{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment ( getArgs )
import Text.Parsec ( runParser, runParserT )
import qualified Data.Map as Map
import Tokenizer ( sample )
import Types (SymTable(SymTable, nameTable, objTable, objCounter))
import Parser (test)
import Control.Monad (void)

main :: IO ()
main = do
    (fp : args) <- getArgs
    src <- readFile fp
    r <- runParserT test (SymTable { nameTable = Map.empty, objTable = Map.empty, objCounter = 0 }) fp src
    print r