module Tokenizer where

import Types
import Text.Parsec.Text ( Parser )
import Text.Parsec
import Control.Monad ( void )
import qualified Data.Map as Map

-- fail :: String -> MainParser ()
-- fail msg = do
--     -- pos <- getPosition
--     updateState $ \st -> ParserState { messages = msg : messages st }

sample :: MainParser SymTable
sample = do
    void $ many1 $ do
        name <- nameParser
        spaces
        void (char '=' <?> "missing =!")
        spaces
        x <- choice [ prim <$> primParser
                    , do
                        str <- strParser
                        cst <- getState
                        let len = Map.size $ objTable cst
                        updateState $ \st -> SymTable { nameTable = nameTable st
                                                      , objTable = Map.insert len (StrObj str) $ objTable st }
                        return $ ref len
                    , do
                        rhsName <- nameParser
                        st <- getState
                        case Map.lookup rhsName $ nameTable st of
                            Just val -> return val
                            _ -> fail $ "Variable \"" ++ rhsName ++ "\" has not been declared"
                        <?> "literal or variable" ]
        void $ char ';'
        updateState $ \st -> SymTable { nameTable = Map.insert name x $ nameTable st 
                                      , objTable = objTable st }
        spaces
    getState

putQuotesAround :: String -> String
putQuotesAround s = '\'' : s ++ "\'"

spaces1 :: Parser ()
spaces1 = space >> spaces 

tabParser :: Parser ()
tabParser = void (try $ string "    ") 
    <|> void (optional (char ' ') >> optional (char ' ') >> optional (char ' ')
        >> char '\t') <?> "tab"

wordParser :: String -> MainParser ()
wordParser s = do
    void $ try $ do
        void $ string s
        notFollowedBy $ char '_' <|> letter <|> digit

nameParser :: MainParser Name
nameParser = do
    x <- char '_' <|> letter
    xs <- many $ char '_' <|> letter <|> digit
    return $ x : xs

primParser :: MainParser Prim -- Could maybe be Raw
primParser = num <|> bool <|> ch <?> "int, float, bool, or char literal, e.g. (67, 3.1415, true, \'k\')"
    where
        bool = true <|> false
            where
                true = do
                    wordParser "true"
                    return $ BoolPrim True
                false = do
                    wordParser "false"
                    return $ BoolPrim False
        num = do
            whole <- many1 digit
            dot <- optionMaybe $ char '.'
            case dot of
                Nothing -> return $ IntPrim $ read whole
                _ -> do
                    decimal <- many1 digit <?> "decimal component, add \'0\' to the end"
                    return $ FloatPrim $ read $ whole ++ "." ++ decimal
        ch = do
            void $ char '\''
            c <- anyChar
            void (char '\'') <?> "closing single quote after char"
            return $ CharPrim c

strParser :: MainParser String -- Could maybe be Obj or Raw
strParser = str <?> "string literal, e.g. \"Hello, World!\""
    where str = do
            void $ char '\"'
            text <- many $ satisfy (/='\"')
            void (char '\"' <?> "quotes after string literal")
            return text

opTokenParser :: Int -> MainParser OpToken
opTokenParser p = case p of
        0 -> do {wordParser "or"; return OrOp} -- wordParser OrOp "or"
        1 -> do {wordParser "and"; return AndOp} -- wordParser AndOp "and"
        2 -> choice [ do {void $ try $ string "=="; return EqualOp}
                    , do {void $ try $ string "!="; return NotEqualOp} ]
        3 -> choice [ do {void $ char '<'; return LessThanOp}
                    , do {void $ char '>'; return GreaterThanOp}
                    , do {void $ try $ string "<="; return LessThanEqualOp}
                    , do {void $ try $ string ">="; return GreaterThanEqualOp} ]
        4 -> choice [ do {wordParser "not"; return NotOp}
                    , do {void $ char '+'; return AddOp}
                    , do {void $ char '-'; return SubOp} ]
        5 -> choice [ do {void $ char '*'; return MulOp}
                    , do {void $ char '/'; return DivOp} 
                    , do {void $ char '%'; return ModOp} ]
        _ -> fail "Invalid operator precedence level"