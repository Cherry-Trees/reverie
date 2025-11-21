{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Types where
import qualified Data.Map as Map
import Text.Parsec ( ParsecT, Parsec )
import Control.Monad.State (StateT)
import Data.Char (ord, chr)
import Text.Parsec.ByteString (Parser)
-- import qualified Data.Vector.Mutable as MV

{- Have a map for names to primitives/references, and a mutable vector for objects created at runtime.
    - Ex. x = 10; would make the entry "x" -> 10 :: Bindable in the first map, and none in the object map (not creating an object here).
    - Ex. x = y = Vec(0,0,0); would make two entries in the name map, and one in the object map.
        name: "x" -> 0 :: Ref
              "y" -> 0 :: Ref
        obj:  0 -> <Vec Obj> Which would maybe have its own name and obj table
-}




{-------------- PARSE TREE --------------}

data StartNode = StartNode { startNodeUses :: [UseNode]
                           , startNodeStmts :: [StmtNode]
                           } deriving Show

newtype UseNode = UseNode { useNames :: [Name] 
                                  } deriving Show -- Could maybe be Name [Name]

data StmtNode
    = ClassStmtNode  ClassNode
    | FnStmtNode     FnNode
    | IfStmtNode     IfNode
    | ForStmtNode    ForNode
    | WhileStmtNode  WhileNode
    | ExprStmtNode   ExprNode

    | ReturnStmtNode ReturnNode
    deriving Show

-- data FnStmtNode
--     = ClassFnStmtNode   ClassNode
--     | FnFnStmtNode      FnNode
--     | IfFnStmtNode      FnIfNode
--     | ForFnStmtNode     FnForNode
--     | WhileFnStmtNode   FnWhileNode
--     | ReturnFnStmtNode  ReturnNode
--     | ExprFnStmtNode    ExprNode

-- newtype BlockNode = [StmtNode] { blockNodeStmts :: [StmtNode] 
--                                     } deriving Show
-- newtype ClassBlockNode = ClassBlockNode { classBlockNodeStmts :: [FnNode]
--                                         } deriving Show
-- newtype FnBlockNode    = FnBlockNode    [FnStmtNode]

data ClassNode = ClassNode { classNodeName   :: Name 
                           , classNodeAttrs  :: [Name]
                           , classNodeParent :: Maybe Name
                           , classNodeBlock  :: [FnNode]
                           } deriving Show

data FnNode = FnNode { fnNodeName   :: Name
                     , fnNodeParams :: [Name]
                     , fnNodeBlock  :: [StmtNode]
                     } deriving Show

data IfNode = IfNode { ifNodeExpr  :: ExprNode
                     , ifNodeBlock :: [StmtNode]
                     , ifNodeElse  :: [StmtNode]
                     } deriving Show

newtype ElseNode = ElseNode { elseNodeBlock :: [StmtNode] 
                            } deriving Show

data ForNode = ForNode { forNodeEnum  :: Name
                       , forNodeExpr  :: ExprNode
                       , forNodeBlock :: [StmtNode]
                       } deriving Show

data WhileNode = WhileNode { whileNodeExpr  :: ExprNode
                           , whileNodeBlock :: [StmtNode]
                           } deriving Show

newtype ReturnNode = ReturnNode { returnNodeExpr :: ExprNode 
                                } deriving Show


-- data FnIfNode = FnIfNode { fnIfNodeExpr  :: ExprNode
--                          , fnIfNodeBlock :: FnBlockNode
--                          , fnIfNodeElse  :: Maybe FnElseNode
--                          }

-- newtype ElseNode = ElseNode { elseNodeBlock :: [StmtNode] 
--                             }

-- data ForNode = ForNode { forNodeEnum  :: Name
--                        , forNodeExpr  :: ExprNode
--                        , forNodeBlock :: [StmtNode]
--                        }

-- data WhileNode = WhileNode { whileNodeExpr  :: ExprNode
--                            , whileNodeBlock :: [StmtNode]
--                            }



-- data BinaryOp5
--     = OrOp
--     deriving Show

-- data BinaryOp4
--     = AndOp
--     deriving Show




data BinaryOp3
    = EqualOp
    | NotEqualOp

data BinaryOp2
    = LessThanOp
    | LessThanEqualOp
    | GreaterThanOp
    | GreaterThanEqualOp

data BinaryOp1
    = AddOp
    | SubOp

data BinaryOp0
    = MulOp
    | DivOp
    | ModOp 

data UnaryOp3 

data UnaryOp2 

data UnaryOp1 

data UnaryOp0
    = NegateOp

instance Show BinaryOp3 where
    show EqualOp = "=="
    show NotEqualOp = "!="

instance Show BinaryOp2 where
    show LessThanOp = "<"
    show LessThanEqualOp = "<="
    show GreaterThanOp = ">"
    show GreaterThanEqualOp = ">="

instance Show BinaryOp1 where
    show AddOp = "+"
    show SubOp = "-"

instance Show BinaryOp0 where
    show MulOp = "*"
    show DivOp = "/"
    show ModOp = "%"

instance Show UnaryOp3 where
    show _ = ""

instance Show UnaryOp2 where
    show _ = ""

instance Show UnaryOp1 where
    show _ = ""

instance Show UnaryOp0 where
    show NegateOp = "-"

data ExprNode
    = AssignExprNode VarNode ExprNode
    | MathExprNode Expr3Node
    deriving Show

data UnaryExpr3Node
    = UnaryExpr3Node (Maybe UnaryOp3) Expr2Node
    deriving Show

data UnaryExpr2Node
    = UnaryExpr2Node (Maybe UnaryOp2) Expr1Node
    deriving Show

data UnaryExpr1Node
    = UnaryExpr1Node (Maybe UnaryOp1) Expr0Node
    deriving Show

data UnaryExpr0Node
    = UnaryExpr0Node (Maybe UnaryOp0) AtomNode
    deriving Show

data Expr3Node 
    = Expr3Node UnaryExpr3Node [(BinaryOp3, UnaryExpr3Node)] 
    deriving Show

data Expr2Node 
    = Expr2Node UnaryExpr2Node [(BinaryOp2, UnaryExpr2Node)] 
    deriving Show

data Expr1Node 
    = Expr1Node UnaryExpr1Node [(BinaryOp1, UnaryExpr1Node)] 
    deriving Show

data Expr0Node 
    = Expr0Node UnaryExpr0Node [(BinaryOp0, UnaryExpr0Node)]  
    deriving Show



-- data Expr0Node
--     = SingleExpr0Node AtomNode
--     | NegExpr0Node AtomNode
--     | MulExpr0Node Expr0Node Expr0Node
--     | DivExpr0Node Expr0Node Expr0Node
--     | ModExpr0Node Expr0Node Expr0Node
--     deriving Show

-- data Expr1Node
--     = SingleExpr1Node Expr0Node
--     | AddExpr1Node Expr1Node Expr1Node
--     | SubExpr1Node Expr1Node Expr1Node
--     deriving Show

-- data Expr2Node
--     = SingleExpr2Node Expr1Node
--     | LTExpr2Node Expr2Node Expr2Node
--     | LTEExpr2Node Expr2Node Expr2Node
--     | GTExpr2Node Expr2Node Expr2Node
--     | GTEExpr2Node Expr2Node Expr2Node
--     deriving Show

-- data Expr3Node
--     = SingleExpr3Node Expr2Node
--     | NEQExpr3Node Expr3Node Expr3Node
--     | EQExpr3Node Expr3Node Expr3Node
--     deriving Show



data AtomNode 
    = VarAtomNode       VarNode
    | BindableAtomNode  Bindable 
    | StrAtomNode       String
    | ListAtomNode      [ExprNode]
    | ExprAtomNode      ExprNode -- '(' expr ')'
    deriving Show

data SuffixNode
    = NameSuffixNode    Name
    | IndexSuffixNode   ExprNode
    | ArgsSuffixNode    [ExprNode]
    deriving Show

data VarNode = VarNode { varNodeName     :: Name 
                       , varNodeSuffixes :: [SuffixNode]
                       } deriving Show



-- If the context is dependent on a type (type-dependent), then it can't be a functor





type Env = StateT SymTable IO
-- type SrcParser = Parser String
-- type MainParser = ParsecT String SymTable IO
data SymTable = SymTable { nameTable  :: NameTable 
                         , objTable   :: ObjTable
                         , objCounter :: Integer
                         } deriving Show

    
type NameTable = Map.Map Name Bindable
type ObjTable = Map.Map Ref Obj
type Ref = Integer
type Name = String


-- Types that can be bound to a name
-- data Bindable
--     = PrimBindable   Bindable
--     | ClassBindable  Class
--     deriving Show

-- Primitive values that can be bound directly to a name
data Bindable
    = RefVal       Ref
    | IntVal       Integer
    | FloatVal     Double
    | BoolVal      Bool
    | CharVal      Char
    | NoneVal

instance Show Bindable where
    show (RefVal x) = 'r' : show x
    show (IntVal x) = show x
    show (FloatVal x) = show x
    show (BoolVal x) = show x
    show (CharVal x) = show x
    show _ = "none"

showPrimType :: Bindable -> String
showPrimType (RefVal _) = "ref"
showPrimType (FloatVal _) = "float"
showPrimType (IntVal _) = "int"
showPrimType (CharVal _) = "char"
showPrimType (BoolVal _) = "bool"
showPrimType _ = "none"


newtype Class = Class { classSymTable :: SymTable } deriving Show

data Obj    -- Built in objects
    = StrObj    String  -- Technically this is just a ListObj, but for the purposes of I/O, this is separated.
    | ListObj   [Bindable]
    | FuncObj   SymTable [StmtNode]-- AST        -- Immutable 
    | ClassObj  SymTable
    deriving Show



-- data Location = Location { locationStartLine    :: Int
--                          , locationStartColumn  :: Int
--                          , locationEndLine      :: Int
--                          , locationEndColumn    :: Int
--                         --  , locationSource       :: Text
--                          } deriving (Show, Typeable, Eq)