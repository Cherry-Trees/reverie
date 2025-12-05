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

-- assignment :: MainParser Prim
-- assignment = do
--     nm <- name
--     tok $ void (char '=' <?> "missing =!")
--     x <- expr
--     updateState $ \st -> SymTable { nameTable = Map.insert nm x $ nameTable st 
--                                     , objTable = objTable st
--                                     , objCounter = objCounter st }
--     return x

-- expr :: MainParser Prim
-- expr = choice [ try assignment
--            , atom ]

-- exprSequence :: MainParser [Prim]
-- exprSequence = option [] $ liftA2 (:) expr $ many $ tok (char ',') >> expr
--     -- e <- atom -- Replace with expr
--     -- es <- many $ do
--     --     void $ char ','
--     --     atom
--     -- return $ e : es
    

-- -- I'm returning all of these Primues straight up from the parsers, but I probably want to build a Tree up.
-- -- This way I don't have to distinguish between if an expression is in a function block or not (can't just return Primues straight up from functions).
-- atom :: MainParser Prim
-- atom = choice [ primParser
--               , RefPrim <$> strObjParser
--               , RefPrim <$> listObj
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
--                         Just Prim -> return Prim
--                         _ -> fail $ "Variable \"" ++ rhsName ++ "\" has not been declared"
--                     <?> "literal or variable" 

-- varParser :: Parser Var
-- varParser = do
--     pf <- name

--     where
--         suffixParser :: Parser Suffix
--         suffixParser = choice [

--                               ]


-- Suffix is either .name , [expr] , (exprSequence)

-- exprParser :: Parser Expr
-- exprParser = choice [ AssignExpr <$> assignExprParser
--                     , MathExpr <$> mathExprParser
--                     ]

--     where
--         assignExprParser :: 

varParser :: Parser Var
varParser = do
    pf <- name
    sf <- many suffixParser
    return $ Var { varName = pf
                 , varSuffixes = sf }
    where 
        suffixParser :: Parser Suffix
        suffixParser = choice [ NameSuffix <$> dotNameParser
                              , IndexSuffix <$> indexParser
                              , ArgsSuffix <$> argsParser ]
            where
                dotNameParser :: Parser Name
                dotNameParser = do
                    ch '.'
                    name <?> "name after dot"

                indexParser :: Parser Expr
                indexParser = do
                    ch '['
                    e <- exprParser <?> "expression after opening bracket in index"
                    ch ']' <?> "closing bracket after index"
                    return e

                argsParser :: Parser [Expr]
                argsParser = do
                    ch '('
                    es <- (exprParser `sepBy` ch ',') <?> "expression list after opening parenthesis in arguments"
                    ch ')' <?> "closing parenthesis after expression sequence in arguments"
                    return es


exprParser :: Parser Expr
exprParser = choice [ try assignExprParser
                    , try ifExprParser
                    , mathExprParser ]
    where 
        assignExprParser :: Parser Expr
        assignExprParser = do
            v <- varParser
            ch '='
            e <- exprParser
            return $ AssignExpr { assignExprLhs = v
                                , assignExprRhs = e
                                }

        ifExprParser :: Parser Expr
        ifExprParser = do
            word "if"
            e <- exprParser
            b <- ctrlBlockParser
            s <- optionMaybe $ word "else"
            b' <- case s of
                Nothing -> return []
                _ -> ctrlBlockParser
            return $ IfExpr { ifExprCond = e
                            , ifExprBlock = b
                            , ifExprElseBlock = b' }

        mathExprParser :: Parser Expr
        mathExprParser = MathExpr <$> orExprParser

        binExprParser :: (a -> [(b, a)] -> c)
                      -> Parser a 
                      -> Parser b 
                      -> Parser c
        binExprParser f ep bp = do
            e <- ep
            es <- many $ liftA2 (,) bp ep
            return $ f e es

        unaryExprParser :: Parser UnaryExpr
        unaryExprParser = do
            us <- many $ choice $ op <$> [NegOp, NotOp]
            a <- atomParser
            -- us' <- many $ choice $ op <$> 
            return $ UnaryExpr us a []  -- Post-fix unary operators will always be empty list unless I implement them.

        orExprParser = binExprParser OrExpr andExprParser orOpParser
        andExprParser = binExprParser AndExpr eqExprParser andOpParser
        eqExprParser = binExprParser EqExpr relExprParser eqOpParser
        relExprParser = binExprParser RelExpr addExprParser relOpParser
        addExprParser = binExprParser AddExpr mulExprParser addOpParser
        mulExprParser = binExprParser MulExpr unaryExprParser mulOpParser

        orOpParser = tok $ op OrOp
        andOpParser = tok $ op AndOp
        eqOpParser = tok $ choice $ op <$> [EqOp, NotEqOp]
        relOpParser = tok $ choice $ op <$> [LTOp, GTOp, LTEOp, GTEOp]
        addOpParser = tok $ choice $ op <$> [AddOp, SubOp]
        mulOpParser = tok $ choice $ op <$> [MulOp, DivOp, ModOp]




                
atomParser :: Parser Atom
atomParser = choice [ PrimAtom <$> primParser
                    , ExprAtom <$> exprAtomParser
                    , StrAtom <$> strObjParser
                    , ListAtom <$> listObjParser
                    , VarAtom <$> varParser ]
            where
                primParser :: Parser Prim
                primParser = choice [ numParser
                                                 , boolParser
                                                 , charParser
                                                 , noneParser
                                                 ] <?> "int, float, bool, char, or none literal, e.g. (67, 3.1415, True, \'k\', None)"
                    where
                        numParser :: Parser Prim
                        numParser = tok $ do
                            whole <- many1 digit
                            dot <- optionMaybe $ char '.'
                            case dot of
                                Nothing -> return $ IntPrim $ read whole
                                _ -> do
                                    decimal <- many1 digit <?> "decimal component, add \'0\' to the end"
                                    return $ FloatPrim $ read $ whole ++ "." ++ decimal

                        boolParser :: Parser Prim
                        boolParser = true <|> false
                            where
                                true = word "True" >> return (BoolPrim True)
                                false = word "False" >> return (BoolPrim False)

                        charParser :: Parser Prim
                        charParser = tok $ do
                            void $ char '\''
                            c <- anyChar
                            void (char '\'') <?> "closing single quote after char"
                            return $ CharPrim c

                        noneParser :: Parser Prim
                        noneParser = word "None" >> return NonePrim 

                exprAtomParser :: Parser Expr
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

                listObjParser :: Parser [Expr]
                listObjParser = do
                    ch '['
                    es <- exprParser `sepBy` ch ','
                    ch ']' <?> "closing bracket after list"
                    return es

                
blockParser :: Parser [Stmt]
blockParser = do 
    ch '{'
    es <- stmtParser `sepEndBy` ch ';'
    ch '}' <?> "closing brace after block"
    return es

-- Fix keywords being assignable
stmtParser :: Parser Stmt
stmtParser = choice [ --IfStmt <$> ifParser
                     ExprStmt <$> exprParser
                    ]

ctrlBlockParser :: Parser [CtrlStmt]
ctrlBlockParser = do
    ch '{'
    es <- ctrlStmtParser `sepEndBy` ch ';'
    ch '}' <?> "closing brace after control flow block"
    return es

ctrlStmtParser :: Parser CtrlStmt
ctrlStmtParser = (GenStmt <$> stmtParser) 
             <|> (BreakStmt <$> (word "break" >> exprParser))
                        
    -- where
    --     breakStmtParser :: Parser Expr
    --     breakStmtParser = 

-- ifParser :: Parser If
-- ifParser = do
--     word "if"
--     e <- exprParser
--     b <- blockParser
--     s <- optionMaybe $ word "else"
--     b' <- case s of
--         Nothing -> return []
--         _ -> blockParser
--     return $ If { ifExpr = e
--                     , ifBlock = b
--                     , ifElseBlock = b' }


-- whileParser :: Parser While
-- whileParser = do
--     word "while"
--     e <- exprParser
--     b <- blockParser
--     return $ While { whileExpr = e
--                        , whileBlock = b }

-- fnParser :: Parser Fn


-- fnParser :: Parser 

test :: Parser [Expr]
test = many $ do
    e <- exprParser
    void $ tok $ char ';'
    return e

test1 :: Parser [Stmt]
test1 = stmtParser `sepEndBy` ch ';'
