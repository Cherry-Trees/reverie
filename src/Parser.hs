module Parser where

import Types
import Tokenizer
import Text.Parsec
import qualified Data.Map as Map
import Control.Monad (void)

insertObj :: Obj -> SymTable -> MainParser Ref
insertObj o st = n <$ updateState (const SymTable { nameTable = nameTable st
                          , objTable = Map.insert n o $ objTable st
                          , objCounter = n + 1 })
                where n = objCounter st

deleteObj :: Ref -> SymTable -> SymTable
deleteObj r st = SymTable { nameTable = nameTable st
                          , objTable = Map.delete r $ objTable st
                          , objCounter = objCounter st }

exprSequenceParser :: MainParser [Raw]
exprSequenceParser = option [] $ liftA2 (:) atomParser $ many (char ',' >> atomParser)
    -- e <- atomParser -- Replace with expr
    -- es <- many $ do
    --     void $ char ','
    --     atomParser
    -- return $ e : es
    
atomParser :: MainParser Raw
atomParser = choice [ prim <$> primParser
                    , ref <$> str
                    , ref <$> list
                    ]
            where 
                str = do
                    s <- strParser
                    cst <- getState
                    insertObj (StrObj s) cst
                list = do
                    void $ char '['
                    es <- exprSequenceParser
                    void (char ']') <?> "closing bracket after list"
                    cst <- getState
                    insertObj (ListObj es) cst


testAtom :: MainParser SymTable
testAtom = do
    void atomParser
    getState