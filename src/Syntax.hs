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
    | EvalM Expr
    deriving (Eq)
instance Show Expr where
    show (Const v) = show v
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (If cnd thn els) = "if " ++ show cnd ++ " then " ++ show thn ++ " else " ++ show els
    show (EvalM e) = "execM " ++ show e
