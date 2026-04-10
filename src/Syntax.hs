module Syntax where

data Value
    = NumV Integer
    | BoolV Bool
    | StrV String
    | ExprV Expr
    deriving (Eq)
instance Show Value where
    show (NumV i) = show i
    show (BoolV True) = "true"
    show (BoolV False) = "false"
    show (StrV str) = str
    show (ExprV e) = "quoted: " ++ show e

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
    | If Expr Expr (Maybe Expr)
    | Quote Expr
    | Eval Expr
    deriving (Eq)
instance Show Expr where
    show (Const v) = show v
    show (BinOp op e0 e1) = show e0 ++ " " ++ show op ++ " " ++ show e1
    show (If cnd thn Nothing) = "if " ++ show cnd ++ ":\n\t" ++ show thn
    show (If cnd thn (Just els)) =
        "if " ++ show cnd ++ ":\n\t" ++ show thn ++ "\nelse:\n\t" ++ show els
    show (Quote e) = "quote(" ++ show e ++ ")"
    show (Eval e) = "eval(" ++ show e ++ ")"
