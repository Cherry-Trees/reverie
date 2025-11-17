module Parser where

import Types
import Tokenizer
import Text.Parsec
import qualified Data.Map as Map
import Control.Monad 

insertObj :: Obj -> SymTable -> MainParser Ref
insertObj o st = n <$ updateState (const SymTable { nameTable = nameTable st
                          , objTable = Map.insert n o $ objTable st
                          , objCounter = n + 1 })
                where n = objCounter st

deleteObj :: Ref -> SymTable -> MainParser ()
deleteObj r st = updateState $ const SymTable { nameTable = nameTable st
                          , objTable = Map.delete r $ objTable st
                          , objCounter = objCounter st }


test1 :: IO ()
test1 = do
    x <- uncons "12345"
    case x of
        Just (c, cs) -> print $ c : cs
        _ -> fail ""

assignment :: MainParser Raw
assignment = do
    nm <- name
    tok $ void (char '=' <?> "missing =!")
    x <- expr
    updateState $ \st -> SymTable { nameTable = Map.insert nm x $ nameTable st 
                                    , objTable = objTable st
                                    , objCounter = objCounter st }
    return x

expr :: MainParser Raw
expr = choice [ try assignment
           , atom ]

exprSequence :: MainParser [Raw]
exprSequence = option [] $ liftA2 (:) expr $ many $ tok (char ',') >> expr
    -- e <- atom -- Replace with expr
    -- es <- many $ do
    --     void $ char ','
    --     atom
    -- return $ e : es
    

-- I'm returning all of these values straight up from the parsers, but I probably want to build a Tree up.
-- This way I don't have to distinguish between if an expression is in a function block or not (can't just return values straight up from functions).
atom :: MainParser Raw
atom = choice [ primToRaw <$> prim
              , refToRaw <$> strObj
              , refToRaw <$> listObj
              , var 
              ] where 
                strObj = do
                    s <- str
                    st <- getState
                    insertObj (StrObj s) st
                listObj = do
                    tok $ void $ char '['
                    es <- exprSequence
                    tok (void $ char ']') <?> "closing bracket after list"
                    st <- getState
                    insertObj (ListObj es) st
                var = do 
                    rhsName <- name
                    st <- getState
                    case Map.lookup rhsName $ nameTable st of
                        Just val -> return val
                        _ -> fail $ "Variable \"" ++ rhsName ++ "\" has not been declared"
                    <?> "literal or variable" 
                
                    


test :: MainParser SymTable
test = do
    void (many (spaces >> expr >> spaces >> char ';'))
    getState