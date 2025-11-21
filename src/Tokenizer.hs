module Tokenizer where

import Types
-- import Text.Parsec.Text ( Parser )
import Text.Parsec
import Control.Monad ( void )
-- import Control.Category ((>>>))
import qualified Data.Map as Map
-- import Text.Parsec.ByteString (Parser)
import Text.Parsec.Text

-- fail :: String -> Parser
-- fail msg = do
--     -- pos <- getPosition
--     updateState $ \st -> ParserState { messages = msg : messages st }



-- sample :: SrcParser SymTable
-- sample = do
--     void $ many1 $ do
--         nm <- name
--         tok $ void (char '=' <?> "missing =!")
--         spaces
--         x <- choice [ basicBindable
--                     , do
--                         s <- str
--                         cst <- getState
--                         let cnt = objCounter cst
--                         updateState $ \st -> SymTable { nameTable = nameTable st
--                                                       , objTable = Map.insert cnt (StrObj s) $ objTable st
--                                                       , objCounter = objCounter st + 1 }
--                         return $ RefType cnt
--                     , do
--                         rhsName <- name
--                         st <- getState
--                         case Map.lookup rhsName $ nameTable st of
--                             Just val -> return val
--                             _ -> fail $ "Variable \"" ++ rhsName ++ "\" has not been declared"
--                         <?> "literal or variable" ]
--         void $ char ';'
--         updateState $ \st -> SymTable { nameTable = Map.insert nm x $ nameTable st 
--                                       , objTable = objTable st
--                                       , objCounter = objCounter st }
--         spaces
--     getState

-- putQuotesAround :: String -> String
-- putQuotesAround s = '\'' : s ++ "\'"

-- spaces1 :: Parser ()
-- spaces1 = space >> spaces 

-- tabParser :: Parser ()
-- tabParser = void (try $ string "    ") 
--     <|> void (optional (char ' ') >> optional (char ' ') >> optional (char ' ')
--         >> char '\t') <?> "tab"

-- tok :: SrcParser a -> SrcParser a
-- tok = (>>) spaces

-- f :: Monad m => m a -> (b -> m b) -> (b -> m a)
-- m () -> (Char -> m ()) -> (Char -> m ())

tok :: Parser a -> Parser a
tok = (>>) spaces . flip (<*) spaces

word :: String -> Parser ()
word s = tok $ void $ try $ do
        void $ string s
        notFollowedBy $ char '_' <|> letter <|> digit

name :: Parser Name
name = tok $ do
    x <- char '_' <|> letter
    xs <- many $ char '_' <|> letter <|> digit
    return $ x : xs

nothing :: Parser a
nothing = fail []

op :: Show a => a -> Parser a
op f = try (string $ show f) >> return f

ch :: Char -> Parser ()
ch = void . tok . char

-- op :: [String -> a] -> Parser a
-- op f s = do



-- op [("==", EqualOp), ("!=", )]


-- operator :: Int -> SrcParser OpToken
-- operator p = case p of
--         0 -> do {word "or"; return OrOp} -- word OrOp "or"
--         1 -> do {word "and"; return AndOp} -- word AndOp "and"
--         2 -> choice [ do {void $ try $ string "=="; return EqualOp}
--                     , do {void $ try $ string "!="; return NotEqualOp} ]
--         3 -> choice [ do {void $ char '<'; return LessThanOp}
--                     , do {void $ char '>'; return GreaterThanOp}
--                     , do {void $ try $ string "<="; return LessThanEqualOp}
--                     , do {void $ try $ string ">="; return GreaterThanEqualOp} ]
--         4 -> choice [ do {word "not"; return NotOp}
--                     , do {void $ char '+'; return AddOp}
--                     , do {void $ char '-'; return SubOp} ]
--         5 -> choice [ do {void $ char '*'; return MulOp}
--                     , do {void $ char '/'; return DivOp} 
--                     , do {void $ char '%'; return ModOp} ]
--         _ -> fail "Invalid operator precedence level"