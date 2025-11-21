module Parser where

import Types
import Tokenizer
import Text.Parsec
import qualified Data.Map as Map
import Control.Monad 
import Text.Parsec.Text





-- insertObj :: Obj -> SymTable -> MainParser Ref
-- insertObj o st = n <$ updateState (const SymTable { nameTable = nameTable st
--                           , objTable = Map.insert n o $ objTable st
--                           , objCounter = n + 1 })
--                 where n = objCounter st

-- deleteObj :: Ref -> SymTable -> MainParser ()
-- deleteObj r st = updateState $ const SymTable { nameTable = nameTable st
--                           , objTable = Map.delete r $ objTable st
--                           , objCounter = objCounter st }


-- test1 :: IO ()
-- test1 = do
--     x <- uncons "12345"
--     case x of
--         Just (c, cs) -> print $ c : cs
--         _ -> fail ""

-- assignment :: MainParser Bindable
-- assignment = do
--     nm <- name
--     tok $ void (char '=' <?> "missing =!")
--     x <- expr
--     updateState $ \st -> SymTable { nameTable = Map.insert nm x $ nameTable st 
--                                     , objTable = objTable st
--                                     , objCounter = objCounter st }
--     return x

-- expr :: MainParser Bindable
-- expr = choice [ try assignment
--            , atom ]

-- exprSequence :: MainParser [Bindable]
-- exprSequence = option [] $ liftA2 (:) expr $ many $ tok (char ',') >> expr
--     -- e <- atom -- Replace with expr
--     -- es <- many $ do
--     --     void $ char ','
--     --     atom
--     -- return $ e : es
    

-- -- I'm returning all of these values straight up from the parsers, but I probably want to build a Tree up.
-- -- This way I don't have to distinguish between if an expression is in a function block or not (can't just return values straight up from functions).
-- atom :: MainParser Bindable
-- atom = choice [ primitiveBindableParser
--               , RefVal <$> strObjParser
--               , RefVal <$> listObj
--               , var 
--               ] where 
--                 strObjParser = do
--                     s <- strObjParser
--                     st <- getState
--                     insertObj (StrObj s) st
--                 listObj = do
--                     ch '['
--                     es <- exprSequence
--                     tok (void $ char ']') <?> "closing bracket after list"
--                     st <- getState
--                     insertObj (ListObj es) st
--                 var = do 
--                     rhsName <- name
--                     st <- getState
--                     case Map.lookup rhsName $ nameTable st of
--                         Just val -> return val
--                         _ -> fail $ "Variable \"" ++ rhsName ++ "\" has not been declared"
--                     <?> "literal or variable" 

-- varParser :: Parser VarNode
-- varParser = do
--     pf <- name

--     where
--         suffixParser :: Parser SuffixNode
--         suffixParser = choice [

--                               ]


-- Suffix is either .name , [expr] , (exprSequence)

-- exprParser :: Parser ExprNode
-- exprParser = choice [ AssignExprNode <$> assignExprParser
--                     , MathExprNode <$> mathExprParser
--                     ]

--     where
--         assignExprParser :: 

varParser :: Parser VarNode
varParser = do
    pf <- name
    sf <- many suffixParser
    return $ VarNode { varNodeName = pf
                     , varNodeSuffixes = sf }
    where 
        suffixParser :: Parser SuffixNode
        suffixParser = choice [ NameSuffixNode <$> dotNameParser
                              , IndexSuffixNode <$> indexParser
                              , ArgsSuffixNode <$> argsParser ]
            where
                dotNameParser :: Parser Name
                dotNameParser = do
                    ch '.'
                    name <?> "name after dot"

                indexParser :: Parser ExprNode
                indexParser = do
                    ch '['
                    e <- exprParser <?> "expression after opening bracket in index"
                    ch ']' <?> "closing bracket after index"
                    return e

                argsParser :: Parser [ExprNode]
                argsParser = do
                    ch '('
                    es <- exprSequenceParser <?> "expression list after opening parenthesis in arguments"
                    ch ')' <?> "closing parenthesis after expression sequence in arguments"
                    return es


exprParser :: Parser ExprNode
exprParser = try assignExprParser <|> mathExprParser 
    where 
        assignExprParser :: Parser ExprNode
        assignExprParser = do
            v <- varParser
            ch '='
            AssignExprNode v <$> exprParser

        mathExprParser :: Parser ExprNode
        mathExprParser = MathExprNode <$> expr3Parser

        exprCata :: (a -> [(b, a)] -> c)
                 -> Parser a 
                 -> Parser b 
                 -> Parser c
        exprCata f ep bp = do
            e <- ep
            es <- many $ liftA2 (,) bp ep
            return $ f e es

        unaryExprCata :: (Maybe a -> b -> c)
                      -> Parser a 
                      -> Parser b 
                      -> Parser c
        unaryExprCata f up ep = do
            mu <- optionMaybe up
            f mu <$> ep

        expr3Parser = exprCata Expr3Node unaryExpr3Parser binaryOp3Parser
        expr2Parser = exprCata Expr2Node unaryExpr2Parser binaryOp2Parser
        expr1Parser = exprCata Expr1Node unaryExpr1Parser binaryOp1Parser
        expr0Parser = exprCata Expr0Node unaryExpr0Parser binaryOp0Parser

        unaryExpr3Parser = unaryExprCata UnaryExpr3Node unaryOp3Parser expr2Parser
        unaryExpr2Parser = unaryExprCata UnaryExpr2Node unaryOp2Parser expr1Parser
        unaryExpr1Parser = unaryExprCata UnaryExpr1Node unaryOp1Parser expr0Parser
        unaryExpr0Parser = unaryExprCata UnaryExpr0Node unaryOp0Parser atomParser

        binaryOp3Parser = tok $ choice $ op <$> [EqualOp, NotEqualOp]
        binaryOp2Parser = tok $ choice $ op <$> [LessThanOp, GreaterThanOp, LessThanEqualOp, GreaterThanEqualOp]
        binaryOp1Parser = tok $ choice $ op <$> [AddOp, SubOp]
        binaryOp0Parser = tok $ choice $ op <$> [MulOp, DivOp, ModOp]

        unaryOp3Parser = nothing
        unaryOp2Parser = nothing
        unaryOp1Parser = nothing
        unaryOp0Parser = op NegateOp
        

exprSequenceParser :: Parser [ExprNode]
exprSequenceParser = option [] $ liftA2 (:) exprParser $ many $ tok (char ',') >> exprParser
                
atomParser :: Parser AtomNode
atomParser = choice [ BindableAtomNode <$> primitiveBindableParser
                    , ExprAtomNode <$> exprAtomParser
                    , StrAtomNode <$> strObjParser
                    , ListAtomNode <$> listObjParser
                    , VarAtomNode <$> varParser ]
            where
                primitiveBindableParser :: Parser Bindable
                primitiveBindableParser = choice [ numParser
                                                 , boolParser
                                                 , charParser
                                                 , noneParser
                                                 ] <?> "int, float, bool, char, or none literal, e.g. (67, 3.1415, True, \'k\', None)"
                    where
                        numParser :: Parser Bindable
                        numParser = tok $ do
                            whole <- many1 digit
                            dot <- optionMaybe $ char '.'
                            case dot of
                                Nothing -> return $ IntVal $ read whole
                                _ -> do
                                    decimal <- many1 digit <?> "decimal component, add \'0\' to the end"
                                    return $ FloatVal $ read $ whole ++ "." ++ decimal

                        boolParser :: Parser Bindable
                        boolParser = true <|> false
                            where
                                true = word "True" >> return (BoolVal True)
                                false = word "False" >> return (BoolVal False)

                        charParser :: Parser Bindable
                        charParser = tok $ do
                            void $ char '\''
                            c <- anyChar
                            void (char '\'') <?> "closing single quote after char"
                            return $ CharVal c

                        noneParser :: Parser Bindable
                        noneParser = word "None" >> return NoneVal 

                exprAtomParser :: Parser ExprNode
                exprAtomParser = do
                    ch '('
                    e <- exprParser
                    ch ')' <?> "closing parenthesis after expression"
                    return e     

                strObjParser :: Parser String
                strObjParser = strObjParser' <?> "string literal, e.g. \"Hello, World!\""
                    where strObjParser' = tok $ do
                            void $ char '\"'
                            text <- many $ satisfy (/='\"')
                            void (char '\"' <?> "quotes after string literal")
                            return text

                listObjParser :: Parser [ExprNode]
                listObjParser = do
                    ch '['
                    es <- exprSequenceParser
                    ch ']' <?> "closing bracket after list"
                    return es

                
blockParser :: Parser [StmtNode]
blockParser = do 
    ch '{'
    es <- many stmtParser
    ch '}' <?> "closing brace after block"
    return es

-- Fix keywords being assignable
stmtParser :: Parser StmtNode
stmtParser = choice [ IfStmtNode <$> ifParser
                    , ExprStmtNode <$> do
                        e <- exprParser
                        ch ';'
                        return e 
                    ]

ifParser :: Parser IfNode
ifParser = do
    word "if"
    e <- exprParser
    b <- blockParser
    s <- optionMaybe $ word "else"
    b' <- case s of
        Nothing -> return []
        _ -> blockParser
    return $ IfNode { ifNodeExpr = e
                    , ifNodeBlock = b
                    , ifNodeElse = b' }



test :: Parser [ExprNode]
test = many $ do
    e <- exprParser
    void $ tok $ char ';'
    return e

test1 :: Parser [StmtNode]
test1 = many stmtParser