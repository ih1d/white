module Eval where

import Syntax

eval :: Expr -> Value
eval (Const v) = v
eval (BinOp o e0 e1) = 
    let (v0, v1) = (eval e0, eval e1)
    in case (o, v0, v1) of
        (Add, NumV n1, NumV n2) -> NumV (n1 + n2)
        (Sub, NumV n1, NumV n2) -> NumV (n1 - n2)
        (Mul, NumV n1, NumV n2) -> NumV (n1 * n2)
        (Pow, NumV n1, NumV n2) -> NumV (n1 ^ n2)
        (And, BoolV b1, BoolV b2) -> BoolV (b1 && b2)
        (Or, BoolV b1, BoolV b2) -> BoolV (b1 || b2)
        (Eq, v0', v1') -> BoolV (v0' == v1')
        (Lt, NumV n1, NumV n2) -> BoolV (n1 < n2)
        (Gt, NumV n1, NumV n2) -> BoolV (n1 > n2)
        (LtEq, NumV n1, NumV n2) -> BoolV (n1 <= n2)
        (GtEq, NumV n1, NumV n2) -> BoolV (n1 >= n2)
        (NotEq, v0', v1') -> BoolV (v0' /= v1')
        _ -> error "type error"
eval (If cnd thn Nothing) = 
    let vc = eval cnd
    in case vc of
        BoolV b -> if b then eval thn else BoolV False
        _ -> error "type error"
eval (If cnd thn (Just els)) = 
    let vc = eval cnd
    in case vc of
        BoolV b -> if b then eval thn else eval els
        _ -> error "type error"
eval (Quote e) = ExprV e
eval (Eval e) = 
    let ve = eval e
    in case ve of
        ExprV e' -> eval e'
        _ -> error "type error"