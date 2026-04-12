module Syntax where

type Ident = String

data Value
    = NumV Integer
    | BoolV Bool
    | StrV String
    deriving (Eq)
instance Show Value where
    show (NumV i) = show i
    show (BoolV True) = "true"
    show (BoolV False) = "false"
    show (StrV str) = str

class ToValue a where
    toValue :: a -> Value

instance ToValue Integer where toValue = NumV
instance ToValue String where toValue = StrV
instance ToValue Bool where toValue = BoolV

data Op
    = Add
    | Sub
    | Mul
    | Pow
    | And
    | Or
    | Eq
    | Lt
    | Gt
    | LtEq
    | GtEq
    | NotEq
    deriving (Eq)
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Pow = "^"
    show And = "and"
    show Or = "or"
    show Eq = "=="
    show Lt = "<"
    show Gt = ">"
    show LtEq = "<="
    show GtEq = ">="
    show NotEq = "!="

data Expr
    = Const Value
    | BinOp Op Expr Expr
    | If Expr Expr Expr
    | Let Ident Expr Expr
    | Var Ident
    | EM Expr
    deriving (Eq)
instance Show Expr where
    show (Const v) = show v
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (If cnd thn els) = "if " ++ show cnd ++ " then " ++ show thn ++ " else " ++ show els
    show (Let v e i) = "let " ++ v ++ " = " ++ show e ++ " in " ++ show i
    show (Var v) = v
    show (EM e) = "em " ++ show e
