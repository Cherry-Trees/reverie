{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment ( getArgs )
import Text.Parsec ( runParser, runParserT, parse )
import qualified Data.Map as Map
-- import Tokenizer ( sample )
-- import Types (SymTable(SymTable, nameTable, objTable, objCounter))
import Parser 
import Control.Monad (void)
import qualified Data.Text as T

main :: IO ()
main = do
    (fp : args) <- getArgs
    src <- readFile fp
    print $ parse test1 fp $ T.pack src
    -- r <- runParserT test (SymTable { nameTable = Map.empty, objTable = Map.empty, objCounter = 0 }) fp src
    -- print r