-- |
-- Module      :  Syntax
-- Copyright   :  (c) Isaac Hiram Lopez Diaz 2026
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  isaac.lopez@upr.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract syntax for the Blue language.
module Syntax where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

-- | The environment
type Env = Map Var Value

-- | A variable name in Blue
type Var = Text

-- | Types in Blue
data Type
    = IntT
    | BoolT
    | StringT
    | ArrowT Type Effect Type
    | ProdT Type Type
    | CodeT Type
    deriving (Eq, Show)

-- | Effect rows
data Effect = Pure | Eff (Set EffLabel) deriving (Eq, Show)

data EffLabel
    = Reflect
    | IO
    | Diverge
    deriving (Eq, Ord, Show)

-- | Values in Blue
data Value
    = IntV Integer
    | BoolV Bool
    | StrV Text
    | PairV Value Value
    | ClosureV Var Type Expr Env
    | CodeV Expr
    deriving (Eq, Show)

-- | Binary Operators
data BinOp
    = Add
    | Sub
    | Mul
    | Pow
    | Eq
    | NotEq
    | Lt
    | Gt
    | LtEq
    | GtEq
    | And
    | Or
    deriving (Eq, Show)

-- | Unary Operators
data UnOp
    = Neg
    | Not
    deriving (Eq, Show)

-- | Pure Blue expressions. Produce a Value when evaluated
data Expr
    = ConstE Value
    | VarE Var
    | BinE BinOp Expr Expr
    | UnE UnOp Expr
    | LamE Var Type Expr -- \(x:τ). e
    | AppE Expr Expr -- e₁ e₂
    | PairE Expr Expr -- (e₁, e₂)
    | FstE Expr -- π₁ e
    | SndE Expr -- π₂ e
    | LetE Var Expr Expr -- let x = e in e
    | IfE Expr Expr Expr -- if e₁ then e₂ else e₃
    | QuoteE Expr -- <| e |>
    | AntiE Expr -- ~ e   (only legal inside QuoteE)
    | EmE Expr -- em e  (reflects Code τ into host; adds Reflect)
    deriving (Eq, Show)
