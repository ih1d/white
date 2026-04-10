module Eval where

import Syntax
import Language.Haskell.Interpreter ( Interpreter, interpret, as, setImports, loadModules, setTopLevelModules )

baseEval :: Expr -> Interpreter Value
baseEval (Const v) = pure v
baseEval (BinOp op e0 e1) = do
    (v0, v1) <- (,) <$> baseEval e0 <*> baseEval e1
    case (op, v0, v1) of
        (Add, NumV a, NumV b)   -> pure $ NumV (a + b)
        (Sub, NumV a, NumV b)   -> pure $ NumV (a - b)
        (Mul, NumV a, NumV b)   -> pure $ NumV (a * b)
        (Pow, NumV a, NumV b)   -> pure $ NumV (a ^ b)
        (And, BoolV a, BoolV b) -> pure $ BoolV (a && b)
        (Or, BoolV a, BoolV b)  -> pure $ BoolV (a || b)
        (Eq, a, b)              -> pure $ BoolV (a == b)
        (NotEq, a, b)           -> pure $ BoolV (a /= b)
        (Lt, NumV a, NumV b)    -> pure $ BoolV (a < b)
        (Gt, NumV a, NumV b)    -> pure $ BoolV (a > b)
        (LtEq, NumV a, NumV b)  -> pure $ BoolV (a <= b)
        (GtEq, NumV a, NumV b)  -> pure $ BoolV (a >= b)
        _                       -> error "type error"
baseEval (If cnd thn els) = do
    vc <- baseEval cnd
    case vc of
        BoolV True -> baseEval thn
        BoolV False -> baseEval els
        _ -> error "type error"
baseEval (EvalM e) = do
    loadModules ["src/Syntax.hs"]
    setTopLevelModules ["Syntax"]
    setImports ["Prelude", "Syntax"]
    v <- interpret (show e) (as :: Expr)
    baseEval v