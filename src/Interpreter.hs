module Interpreter where
import Types
import Control.Monad.State 
import qualified Data.Map as Map

insertObj :: Obj -> PrgmState Ref
insertObj o = do
    st <- get
    let n = objCounter st
    put $ SymTable { objCounter = n + 1
                   , objTable = Map.insert n o (objTable st)
                   , nameTable = nameTable st }
    return n

getObj :: Ref -> PrgmState Obj
getObj r = do
    ot <- gets objTable
    case Map.lookup r ot of
        Just o -> return o
        _ -> liftIO $ fail $ "Object does not exist at address " ++ show r

class AST a where
    eval :: a -> PrgmState Raw

instance AST VarNode where
    eval v = do 
        nt <- gets nameTable
        let nm = varNodeName v
        case Map.lookup nm nt of 
            Just x -> return x
            _ -> liftIO $ fail $ "Variable " ++ show nm ++ " undefined"
                -- liftIO $ throwError (userError "")
            -- _ -> fail "Variable undefined"

instance AST AtomNode where
    eval (VarAtomNode v) = eval v
    eval (PrimAtomNode p) = return $ primToRaw p
    eval (StrAtomNode s) = refToRaw <$> insertObj (StrObj s)
    eval (ListAtomNode l) = do
        xs <- mapM eval l
        r <- insertObj $ ListObj xs
        return $ refToRaw r

instance AST ExprNode where