module Interpreter where
import Types
import Control.Monad.State 
import qualified Data.Map as Map
import Control.Exception (throwIO)
import Control.Monad.Catch (catchIOError)
import Control.Monad (void, msum)
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

insertObj :: Obj -> Prgm ()
insertObj o = do
    st <- lift get
    let cr = heapCurrRef st
    lift $ put $ Heap { heapObjTable = Map.insert cr o $ heapObjTable st 
                      , heapCurrRef = cr + 1 }

lookupObj :: Ref -> Prgm (Maybe Obj)
lookupObj r = do
    ot <- lift $ gets heapObjTable
    case Map.lookup r ot of
        Just o -> return $ Just o
        _ -> return Nothing

lookupSym :: Name -> Prgm (Maybe Bindable)
lookupSym n = do
    st <- get
    return $ msum $ Map.lookup n <$> st


-- evalVarRef :: VarNode -> Prgm Bindable
-- evalVarRef vn = do
--     mx <- lookupSym $ varNodeName vn
--     case mx of
--         Just (RefVal r)
--         Just x -> 

--         _ -> liftIO $ fail ""
    





-- throwPrgmError :: String -> Env a
-- throwPrgmError = liftIO . fail



-- -- orMacro :: Bindable -> Bindable -> Env Bindable
-- -- orMacro x1 x2 = case (x1, x2) of
-- --     (Left p1, Left p2) -> case (p1, p2) of
-- --         (BoolPrim b1, BoolPrim b2) -> return $ primToRaw $ BoolPrim $ b1 || b2
-- --         _ -> throwPrgmError $ show p1 ++ " and " ++ show p2 ++ " have incompatible types (should be Bool or Bool)"
--     -- (r1, ) -> throwPrgmError $ show p1 ++ " and " ++ show p2 ++ " have incompatible types (should be Bool or Bool)"


        

--     -- (Right r1, Right r2) -> do

-- -- binaryOpMacro :: (a -> a -> a) -> Bindable -> Bindable -> Env Bindable
-- -- binaryOpMacro f 


-- -- class Macro a where
-- --     arity :: a -> Int
-- --     encode :: a -> (b -> Env Bindable)

-- -- instance Macro BinaryOp0 where
-- --     arity = const 2
-- --     encode MulOp = 


-- negateMacro :: Bindable -> Env Bindable
-- negateMacro (IntVal a) = return $ IntVal $ negate a
-- negateMacro (FloatVal a) = return $ FloatVal $ negate a
-- negateMacro (BoolVal a) = return $ BoolVal $ not a
-- negateMacro a = throwPrgmError $ "Cannot negate " ++ show a

-- mulMacro :: Bindable -> Bindable -> Env Bindable
-- mulMacro (IntVal a) (IntVal b) = return $ IntVal $ a * b
-- mulMacro (FloatVal a) (FloatVal b) = return $ FloatVal $ a * b
-- mulMacro a b = throwPrgmError $ "Cannot multiply " ++ show a ++ " and " ++ show b

-- -- divMacro :: Bindable -> Bindable -> Env Bindable
-- -- divMacro (IntVal a) (IntVal b) = return $ IntVal $ a / b
-- -- divMacro (FloatVal a) (FloatVal b) = return $ FloatVal $ a / b
-- -- divMacro a b = throwPrgmError $ "Cannot divide " ++ show a ++ " and " ++ show b

-- modMacro :: Bindable -> Bindable -> Env Bindable
-- modMacro (IntVal a) (IntVal b) = return $ IntVal $ a `mod` b
-- modMacro a b = throwPrgmError $ "Cannot mod " ++ show a ++ " and " ++ show b



-- class AST a where
--     eval :: a -> Env Bindable


-- instance AST VarNode where
--     eval v = do 
--         nt <- gets nameTable
--         let nm = varNodeName v
--         case Map.lookup nm nt of 
--             Just x -> return x
--             _ -> throwPrgmError $ "Variable " ++ show nm ++ " undefined"

-- instance AST ExprNode where
-- --     eval (AssignExprNode v e) = do
-- --         er <- eval e
-- --         vr <- eval v `catchIOError`
        


-- instance AST StmtNode where
-- --    = ClassStmtNode  ClassNode
-- --     | FnStmtNode     FnNode
-- --     | IfStmtNode     IfNode
-- --     | ForStmtNode    ForNode
-- --     | WhileStmtNode  WhileNode
-- --     | ExprStmtNode   ExprNode
-- --     | ReturnStmtNode ReturnNode
--     eval (ExprStmtNode e) = eval e
--     eval (IfStmtNode i) = do
        

--     eval _ = throwPrgmError "stmt type not implemented yet"


-- instance AST IfNode where
--     eval i = do
--         e <- eval $ ifNodeExpr i
--         case e of
--             (BoolVal bv) -> do 
--                 mapM_ eval $ if bv
--                     then ifNodeBlock i
--                     else ifNodeElseBlock i
--                 return NoneVal
--             _ -> throwPrgmError "Type error"



                

                -- liftIO $ throwError (userError "")
            -- _ -> fail "Variable undefined"

-- instance AST AtomNode where
--     eval (VarAtomNode v) = eval v
--     eval (BindableAtomNode p) = return p
--     eval (StrAtomNode s) = RefVal <$> insertObj (StrObj s)
--     eval (ListAtomNode l) = do
--         xs <- mapM eval l
--         r <- insertObj $ ListObj xs
--         return $ RefVal r
--     eval (ExprAtomNode e) = eval e




-- instance AST Expr0Node where
--     eval (Expr0Node u a s) = do
--         r <- case u of
--             Just NegateOp -> negateMacro =<< p
--             _ -> p
--             `catchIOError` handler u
--         case s of
--             (Just (o0, e0)) -> do
--                 r' <- eval e0
--                 case o0 of
--                     MulOp -> r `mulMacro` r' 
--                     DivOp -> r `divMacro` r'
--                     ModOp -> r `modMacro` r'
--                 `catchIOError` handler o0
--             _ -> return r
--         where 
--             p = eval a

--             handler :: Show a => a -> IOError -> Env Bindable
--             handler o e = throwPrgmError $ "Type mismatch arising from " ++ show o ++ " operation: " ++ show e


-- instance AST ExprNode where
--     eval (AssignExpr v e) = do
--         r <- eval e
--         st <- get
--         put $ SymTable { objCounter = objCounter st
--                        , objTable = objTable st
--                        , nameTable = Map.insert (varNodeName v) r $ nameTable st }
--         return r
--     eval (MathExpr e3) = eval e3

-- instance AST Expr3Node where
--     eval (Expr3Node e2 s) = do
--         r <- eval e2
--         case s of
--             Just (o3, e3) -> do
--                 r' <- eval e3
--                 case o3 of
--                     EqualOp -> 
                
--                 -- case (r1, r2) of
--                 --     ()
--             _ -> return r1

