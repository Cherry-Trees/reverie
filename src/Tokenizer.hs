module Tokenizer where

import Types
-- import Text.Parsec.Text ( Parser )
import Text.Parsec
import Control.Monad ( void )
-- import Control.Category ((>>>))
import qualified Data.Map as Map

-- fail :: String -> MainParser ()
-- fail msg = do
--     -- pos <- getPosition
--     updateState $ \st -> ParserState { messages = msg : messages st }



sample :: MainParser SymTable
sample = do
    void $ many1 $ do
        nm <- name
        tok $ void (char '=' <?> "missing =!")
        spaces
        x <- choice [ primToRaw <$> prim
                    , do
                        s <- str
                        cst <- getState
                        let cnt = objCounter cst
                        updateState $ \st -> SymTable { nameTable = nameTable st
                                                      , objTable = Map.insert cnt (StrObj s) $ objTable st
                                                      , objCounter = objCounter st + 1 }
                        return $ refToRaw cnt
                    , do
                        rhsName <- name
                        st <- getState
                        case Map.lookup rhsName $ nameTable st of
                            Just val -> return val
                            _ -> fail $ "Variable \"" ++ rhsName ++ "\" has not been declared"
                        <?> "literal or variable" ]
        void $ char ';'
        updateState $ \st -> SymTable { nameTable = Map.insert nm x $ nameTable st 
                                      , objTable = objTable st
                                      , objCounter = objCounter st }
        spaces
    getState

-- putQuotesAround :: String -> String
-- putQuotesAround s = '\'' : s ++ "\'"

-- spaces1 :: Parser ()
-- spaces1 = space >> spaces 

-- tabParser :: Parser ()
-- tabParser = void (try $ string "    ") 
--     <|> void (optional (char ' ') >> optional (char ' ') >> optional (char ' ')
--         >> char '\t') <?> "tab"

-- tok :: MainParser a -> MainParser a
-- tok = (>>) spaces

-- f :: Monad m => m a -> (b -> m b) -> (b -> m a)
-- m () -> (Char -> m ()) -> (Char -> m ())

tok :: MainParser a -> MainParser a
tok = (>>) spaces . flip (<*) spaces

-- charP :: Char -> MainParser ()
-- charP = tok . void . char

word :: String -> MainParser ()
word s = tok $ void $ try $ do
        void $ string s
        notFollowedBy $ char '_' <|> letter <|> digit

name :: MainParser Name
name = tok $ do
    x <- char '_' <|> letter
    xs <- many $ char '_' <|> letter <|> digit
    return $ x : xs

prim :: MainParser Prim -- Could maybe be Raw
prim = num <|> bool <|> ch <?> "int, float, bool, or char literal, e.g. (67, 3.1415, true, \'k\')"
    where
        num = tok $ do
            whole <- many1 digit
            dot <- optionMaybe $ char '.'
            case dot of
                Nothing -> return $ IntPrim $ read whole
                _ -> do
                    decimal <- many1 digit <?> "decimal component, add \'0\' to the end"
                    return $ FloatPrim $ read $ whole ++ "." ++ decimal
        bool = true <|> false
            where
                true = word "true" >> return (BoolPrim True)
                false = word "false" >> return (BoolPrim False)
        ch = tok $ do
            void $ char '\''
            c <- anyChar
            void (char '\'') <?> "closing single quote after char"
            return $ CharPrim c

str :: MainParser String -- Could maybe be Obj or Raw
str = str' <?> "string literal, e.g. \"Hello, World!\""
    where str' = tok $ do
            void $ char '\"'
            text <- many $ satisfy (/='\"')
            void (char '\"' <?> "quotes after string literal")
            return text

operator :: Int -> MainParser OpToken
operator p = case p of
        0 -> do {word "or"; return OrOp} -- word OrOp "or"
        1 -> do {word "and"; return AndOp} -- word AndOp "and"
        2 -> choice [ do {void $ try $ string "=="; return EqualOp}
                    , do {void $ try $ string "!="; return NotEqualOp} ]
        3 -> choice [ do {void $ char '<'; return LessThanOp}
                    , do {void $ char '>'; return GreaterThanOp}
                    , do {void $ try $ string "<="; return LessThanEqualOp}
                    , do {void $ try $ string ">="; return GreaterThanEqualOp} ]
        4 -> choice [ do {word "not"; return NotOp}
                    , do {void $ char '+'; return AddOp}
                    , do {void $ char '-'; return SubOp} ]
        5 -> choice [ do {void $ char '*'; return MulOp}
                    , do {void $ char '/'; return DivOp} 
                    , do {void $ char '%'; return ModOp} ]
        _ -> fail "Invalid operator precedence level"