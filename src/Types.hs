{-# LANGUAGE InstanceSigs #-}

module Types where
import qualified Data.Map as Map
import Text.Parsec ( ParsecT )
import Control.Monad.State (StateT)
-- import qualified Data.Vector.Mutable as MV

{- Have a map for names to primitives/references, and a mutable vector for objects created at runtime.
    - Ex. x = 10; would make the entry "x" -> 10 :: Prim in the first map, and none in the object map (not creating an object here).
    - Ex. x = y = Vec(0,0,0); would make two entries in the name map, and one in the object map.
        name: "x" -> 0 :: Ref
              "y" -> 0 :: Ref
        obj:  0 -> <Vec Obj> Which would maybe have its own name and obj table
-}

{-------------- PARSE TREE --------------}

data StartNode = StartNode { startNodeUseStmts :: [UseStmtNode]
                           , startNodeGenStmts :: [GenStmtNode]
                           } deriving Show

newtype UseStmtNode = UseStmtNode { useStmtNames :: [Name] 
                                  } deriving Show -- Could maybe be Name [Name]

data GenStmtNode
    = ClassGenStmtNode  ClassNode
    | FnGenStmtNode     FnNode
    | IfGenStmtNode     IfNode
    | ForGenStmtNode    ForNode
    | WhileGenStmtNode  WhileNode
    | ExprGenStmtNode   ExprNode

    | ReturnGenStmtNode ReturnNode
    deriving Show

-- data FnStmtNode
--     = ClassFnStmtNode   ClassNode
--     | FnFnStmtNode      FnNode
--     | IfFnStmtNode      FnIfNode
--     | ForFnStmtNode     FnForNode
--     | WhileFnStmtNode   FnWhileNode
--     | ReturnFnStmtNode  ReturnNode
--     | ExprFnStmtNode    ExprNode

newtype GenBlockNode = GenBlockNode { genBlockNodeStmts :: [GenStmtNode] 
                                    } deriving Show
newtype ClassBlockNode = ClassBlockNode { classBlockNodeStmts :: [FnNode]
                                        } deriving Show
-- newtype FnBlockNode    = FnBlockNode    [FnStmtNode]

data ClassNode = ClassNode { classNodeName   :: Name 
                           , classNodeAttrs  :: [Name]
                           , classNodeParent :: Maybe Name
                           , classNodeBlock  :: ClassBlockNode
                           } deriving Show

data FnNode = FnNode { fnNodeName   :: Name
                     , fnNodeParams :: [Name]
                     , fnNodeBlock  :: GenBlockNode
                     } deriving Show

data IfNode = IfNode { ifNodeExpr  :: ExprNode
                     , ifNodeBlock :: GenBlockNode
                     , ifNodeElse  :: Maybe ElseNode
                     } deriving Show

newtype ElseNode = ElseNode { elseNodeBlock :: GenBlockNode 
                            } deriving Show

data ForNode = ForNode { forNodeEnum  :: Name
                       , forNodeExpr  :: ExprNode
                       , forNodeBlock :: GenBlockNode
                       } deriving Show

data WhileNode = WhileNode { whileNodeExpr  :: ExprNode
                           , whileNodeBlock :: GenBlockNode
                           } deriving Show

newtype ReturnNode = ReturnNode { returnNodeExpr :: ExprNode 
                                } deriving Show


-- data FnIfNode = FnIfNode { fnIfNodeExpr  :: ExprNode
--                          , fnIfNodeBlock :: FnBlockNode
--                          , fnIfNodeElse  :: Maybe FnElseNode
--                          }

-- newtype ElseNode = ElseNode { elseNodeBlock :: GenBlockNode 
--                             }

-- data ForNode = ForNode { forNodeEnum  :: Name
--                        , forNodeExpr  :: ExprNode
--                        , forNodeBlock :: GenBlockNode
--                        }

-- data WhileNode = WhileNode { whileNodeExpr  :: ExprNode
--                            , whileNodeBlock :: GenBlockNode
--                            }



data BinaryOp5
    = OrOp
    deriving Show

data BinaryOp4
    = AndOp
    deriving Show

data BinaryOp3
    = EqualOp
    | NotEqualOp
    deriving Show

data BinaryOp2
    = LessThanOp
    | LessThanEqualOp
    | GreaterThanOp
    | GreaterThanEqualOp
    deriving Show

data BinaryOp1
    = AddOp
    | SubOp
    deriving Show

data BinaryOp0
    = MulOp
    | DivOp
    | ModOp 
    deriving Show

data UnaryOp3
    = NotOp
    deriving Show

data UnaryOp0
    = NegateOp
    deriving Show

data ExprNode
    = AssignExpr VarNode ExprNode
    | MathExpr Expr5Node
    deriving Show

data Expr5Node = Expr5Node                  Expr4Node [(BinaryOp5, Expr4Node)] deriving Show
data Expr4Node = Expr4Node                  Expr3Node [(BinaryOp4, Expr3Node)] deriving Show
data Expr3Node = Expr3Node (Maybe UnaryOp3) Expr2Node [(BinaryOp3, Expr2Node)] deriving Show
data Expr2Node = Expr2Node                  Expr1Node [(BinaryOp2, Expr1Node)] deriving Show
data Expr1Node = Expr1Node                  Expr0Node [(BinaryOp1, Expr0Node)] deriving Show
data Expr0Node = Expr0Node (Maybe UnaryOp0)  AtomNode [(BinaryOp0,  AtomNode)] deriving Show

data AtomNode 
    = VarAtomNode   VarNode
    | PrimAtomNode  Prim 
    | StrAtomNode   String
    | ListAtomNode  [ExprNode]
    deriving Show

newtype VarNode = VarNode { varNodeName :: Name 
                          } deriving Show



type PrgmState = StateT SymTable IO
type TokenParser = ParsecT String StartNode IO
type MainParser = ParsecT String SymTable IO
data SymTable = SymTable { nameTable  :: NameTable 
                         , objTable   :: ObjTable
                         , objCounter :: Integer
                         } deriving Show
type NameTable = Map.Map Name Raw
type ObjTable = Map.Map Ref Obj
-- type ObjTable s = MV.MVector s Obj

type Ref = Integer
type Name = String
type Raw = Either Prim Ref

primToRaw :: Prim -> Raw
primToRaw = Left

refToRaw :: Ref -> Raw
refToRaw = Right

data Prim
    = IntPrim   Integer
    | FloatPrim Double
    | BoolPrim  Bool
    | CharPrim  Char
    | NonePrim
    deriving Eq

instance Show Prim where
    show (IntPrim x) = show x
    show (FloatPrim x) = show x
    show (BoolPrim x) = show x
    show (CharPrim x) = show x
    show _ = "None"

newtype Class = Class { classSymTable :: SymTable }

data Obj    -- Built in objects
    = StrObj    String  -- Technically this is just a ListObj, but for the purposes of I/O, this is separated.
    | ListObj   [Raw]
    | FuncObj   SymTable GenBlockNode-- AST        -- Immutable 
    deriving Show

-- data Oper
--     = AssignOper
--     | AddOper
--     | SubOper
--     | MulOper
--     | DivOper
--     | ModOper
--     | AndOper
--     | OrOper
--     | LessThanOper
--     | LessThanEqualOper
--     | GreaterThanOper
--     | GreaterThanEqualOper
--     | EqualOper
--     | NotEqualOper
--     | NotOper
--     deriving (Show, Eq)





-- data WordToken
--     = IfWord
--     | ElseWord
--     | ForWord
--     | WhileWord
--     | FnWord
--     | ReturnWord
--     | ClassWord
--     | PubWord
--     | PrivWord
--     deriving (Show, Eq)

-- type SymbolTable = Map.Map Name Val

-- data Val
--     = PrimVal Prim
--     | ListVal [Val]
--     | FuncVal SymbolTable AST

-- data Token
--     = Identifier    String
--     | IntPrim    Int
--     | FloatLiteral  Double
--     | StrLiteral    String
--     | AssignOp
--     | AddOp
--     | SubOp
--     | MulOp
--     | DivOp
--     | ModOp
--     | AndWord
--     | OrWord
--     | LessThanOp
--     | LessThanEqualOp
--     | GreaterThanOp
--     | GreaterThanEqualOp
--     | EqualOp
--     | NotEqualOp
--     | NotWord
--     | Colon
--     | Dot
--     | Comma
--     | LeftParenthesis
--     | RightParenthesis
--     | LeftBracket
--     | RightBracket
--     | LeftBrace 
--     | RightBrace
--     | IfWord
--     | ElseWord
--     | ForWord
--     | WhileWord
--     | FnWord
--     | ReturnWord
--     | ClassWord
--     | PubWord
--     | PrivWord
--     | Tab
--     | Newline
--     deriving Eq

-- class Token where


-- instance Show Token where
--     show (Identifier s)         = "identifier \'" ++ s ++ "\'"
--     show (IntPrim i)         = "int literal \'" ++ show i ++ "\'"
--     show (FloatLiteral f)       = "float literal \'" ++ show f ++ "\'"
--     show (StrLiteral s)         = "string literal \'" ++ s ++ "\'"
--     show AssignOp               = "\'=\'"
--     show AddOp                  = "\'+\'"
--     show SubOp                  = "\'-\'"
--     show MulOp                  = "\'*\'"
--     show DivOp                  = "\'/\'"
--     show ModOp                  = "\'%\'"
--     show LessThanOp             = "\'<\'"
--     show LessThanEqualOp        = "\'<=\'"
--     show GreaterThanOp          = "\'>\'"
--     show GreaterThanEqualOp     = "\'>=\'"
--     show EqualOp                = "\'==\'"
--     show NotEqualOp             = "\'!=\'"
--     show Colon                  = "\':\'"
--     show Dot                    = "\'.\'"
--     show Comma                  = "\',\'"
--     show LeftParenthesis        = "\'(\'"
--     show RightParenthesis       = "\')\'"
--     show LeftBracket            = "\'[\'"
--     show RightBracket           = "\']\'"
--     show LeftBrace              = "\'{\'"
--     show RightBrace             = "\'}\'"
--     show AndWord                = "keyword \'and\'"
--     show OrWord                 = "keyword \'or\'"
--     show NotWord                = "keyword \'not\'"
--     show IfWord                 = "keyword \'if\'"
--     show ElseWord               = "keyword \'else\'"
--     show ForWord                = "keyword \'for\'"
--     show WhileWord              = "keyword \'while\'"
--     show FnWord                 = "keyword \'fn\'"
--     show ReturnWord             = "keyword \'return\'" 
--     show ClassWord              = "keyword \'class\'"
--     show PubWord                = "keyword \'pub\'"
--     show PrivWord               = "keyword \'priv\'"
--     show Tab                    = "tab"
--     show Newline                = "newline"

-- data Location = Location { locationStartLine    :: Int
--                          , locationStartColumn  :: Int
--                          , locationEndLine      :: Int
--                          , locationEndColumn    :: Int
--                         --  , locationSource       :: Text
--                          } deriving (Show, Typeable, Eq)