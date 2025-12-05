{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Types where
import qualified Data.Map as Map
import Text.Parsec ( ParsecT, Parsec )
import Control.Monad.State
import Data.Char (ord, chr)
import Text.Parsec.ByteString (Parser)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MV
-- import qualified Data.Vector.Mutable as MV

{- Have a map for names to primitives/references, and a mutable vector for objects created at runtime.
    - Ex. x = 10; would make the entry "x" -> 10 :: Prim in the first map, and none in the object map (not creating an object here).
    - Ex. x = y = Vec(0,0,0); would make two entries in the name map, and one in the object map.
        name: "x" -> 0 :: Ref
              "y" -> 0 :: Ref
        obj:  0 -> <Vec Obj> Which would maybe have its own name and obj table
-}




{-------------- PARSE TREE --------------}




data Start = Start { startUses :: [Name]
                   , startDecls :: [Decl]
                   } deriving Show

data Decl
    = RecDecl   Rec
    | FnDecl    Fn
    deriving Show


-- data Stmt
--     = GenStmt   Stmt
--     | BreakStmt Expr
--     deriving Show

data Rec = Rec { className   :: Name 
               , classAttrs  :: [Name]
               , classParent :: Maybe Name
               , classBlock  :: [Fn]
               } deriving Show

data Fn = Fn { fnName   :: Name
             , fnParams :: [Name]
             , fnExpr   :: Expr
             } deriving Show


-- data Block = Block { blockStmts :: [Stmt]
--                    , blockEnd :: Maybe Expr  -- Expression without semicolon at the end.
--                    }

data Expr
    = MathExpr      OrExpr
    | AssignExpr    { assignExprLhs         :: Var
                    , assignExprRhs         :: Expr
                    }
    deriving Show

data OrExpr
    = OrExpr AndExpr [(OrPrecOp, AndExpr)]
    deriving Show

data AndExpr
    = AndExpr EqExpr [(AndPrecOp, EqExpr)]
    deriving Show

data EqExpr 
    = EqExpr RelExpr [(EqPrecOp, RelExpr)] 
    deriving Show

data RelExpr 
    = RelExpr AddExpr [(RelPrecOp, AddExpr)] 
    deriving Show

data AddExpr 
    = AddExpr MulExpr [(AddPrecOp, MulExpr)] 
    deriving Show

data MulExpr 
    = MulExpr UnaryExpr [(MulPrecOp, UnaryExpr)]  
    deriving Show

data UnaryExpr
    = UnaryExpr [PreUnaryOp] Atom [PostUnaryOp]
    deriving Show

data Atom 
    = VarAtom   Var
    | PrimAtom  Prim 
    | StrAtom   String
    | ListAtom  [Expr]
    | BlockAtom [Expr]
    | TupleAtom [Expr]
    deriving Show

data Suffix
    = NameSuffix    Name
    | ArgsSuffix    [Expr]
    deriving Show

data Var = Var { varName     :: Name 
               , varSuffixes :: [Suffix]
               } deriving Show


data PreUnaryOp
    = NegOp
    | NotOp

data PostUnaryOp

data OrPrecOp
    = OrOp

data AndPrecOp
    = AndOp

data EqPrecOp
    = EqOp
    | NotEqOp

data RelPrecOp
    = LTOp
    | LTEOp
    | GTOp
    | GTEOp

data AddPrecOp
    = AddOp
    | SubOp

data MulPrecOp
    = MulOp
    | DivOp
    | ModOp 

instance Show PreUnaryOp where
    show NegOp = "-"
    show NotOp = "!"

instance Show PostUnaryOp where
    show _ = ""

instance Show OrPrecOp where
    show OrOp = "||"

instance Show AndPrecOp where
    show AndOp = "&&"

instance Show EqPrecOp where
    show EqOp = "=="
    show NotEqOp = "!="

instance Show RelPrecOp where
    show LTOp = "<"
    show LTEOp = "<="
    show GTOp = ">"
    show GTEOp = ">="

instance Show AddPrecOp where
    show AddOp = "+"
    show SubOp = "-"

instance Show MulPrecOp where
    show MulOp = "*"
    show DivOp = "/"
    show ModOp = "%"



-- If the context is dependent on a type (type-dependent), then it can't be a functor


-- data SymInfo = SymInfo {

--                        }

type Prgm = StateT SymTable (StateT Heap IO)
data Heap = Heap { heapObjTable :: Map.Map Ref Obj
                 , heapCurrRef :: Ref }

type Scope = Map.Map Name Prim
type SymTable = [Scope]

type Ref = Integer
type Name = String


-- Types that can be bound to a name
-- data Prim
--     = PrimBindable   Prim
--     | ClassBindable  Class
--     deriving Show


-- Prim Primues that can be bound directly to a name
data Prim
    = RefPrim       Ref
    | IntPrim       Integer
    | FloatPrim     Double
    | BoolPrim      Bool
    | CharPrim      Char
    | NonePrim

instance Show Prim where
    show (RefPrim x) = 'r' : show x
    show (IntPrim x) = show x
    show (FloatPrim x) = show x
    show (BoolPrim x) = show x
    show (CharPrim x) = show x
    show _ = "None"

showPrimType :: Prim -> String
showPrimType (RefPrim _) = "Ref"
showPrimType (FloatPrim _) = "Float"
showPrimType (IntPrim _) = "Int"
showPrimType (CharPrim _) = "Char"
showPrimType (BoolPrim _) = "Bool"
showPrimType _ = "None"


newtype Class = Class { classSymTable :: SymTable } deriving Show

data Obj    -- Built in objects
    = StrObj    String  -- Technically this is just a ListObj, but for the purposes of I/O, this is separated.
    | ListObj   [Prim]
    | MapObj    (Map.Map Prim Prim)
    | TupleObj  (Vec.Vector Prim)
    | RecObj    SymTable
    -- | FuncObj   SymTable [Stmt]-- AST        -- Immutable 
    deriving Show


-- data Location = Location { locationStartLine    :: Int
--                          , locationStartColumn  :: Int
--                          , locationEndLine      :: Int
--                          , locationEndColumn    :: Int
--                         --  , locationSource       :: Text
--                          } deriving (Show, Typeable, Eq)