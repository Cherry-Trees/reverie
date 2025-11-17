module Types where
import qualified Data.Map as Map
import Text.Parsec ( ParsecT )
-- import qualified Data.Vector.Mutable as MV

{- Have a map for names to primitives/references, and a mutable vector for objects created at runtime.
    - Ex. x = 10; would make the entry "x" -> 10 :: Prim in the first map, and none in the object map (not creating an object here).
    - Ex. x = y = Vec(0,0,0); would make two entries in the name map, and one in the object map.
        name: "x" -> 0 :: Ref
              "y" -> 0 :: Ref
        obj:  0 -> <Vec Obj> Which would maybe have its own name and obj table
-}

type MainParser = ParsecT String SymTable IO
data SymTable = SymTable { nameTable :: NameTable 
                         , objTable :: ObjTable
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
    deriving Eq

instance Show Prim where
    show (IntPrim x) = show x
    show (FloatPrim x) = show x
    show (BoolPrim x) = show x
    show (CharPrim x) = show x

newtype Class = Class { classSymTable :: SymTable }

data Obj    -- Built in objects
    = StrObj    String  -- Technically this is just a ListObj, but for the purposes of I/O, this is separated.
    | ListObj   [Raw]
    | FuncObj   SymTable -- AST        -- Immutable 
    deriving Show

data OpToken
    = AssignOp
    | AddOp
    | SubOp
    | MulOp
    | DivOp
    | ModOp
    | AndOp
    | OrOp
    | LessThanOp
    | LessThanEqualOp
    | GreaterThanOp
    | GreaterThanEqualOp
    | EqualOp
    | NotEqualOp
    | NotOp
    deriving (Show, Eq)

data WordToken
    = IfWord
    | ElseWord
    | ForWord
    | WhileWord
    | FnWord
    | ReturnWord
    | ClassWord
    | PubWord
    | PrivWord
    deriving (Show, Eq)

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