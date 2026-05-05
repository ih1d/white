{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Monad where

import Syntax
import Control.Monad.State
import Language.Haskell.Interpreter (Interpreter)

type Env = [(Ident, Value)]

newtype Eval a = Eval { runEval :: StateT Env Interpreter a }
    deriving (Functor, Applicative, Monad, MonadState Env, MonadIO)

bindVar :: Ident -> Value -> Eval ()
bindVar v val = do
    env <- get
    case lookup v env of
        Nothing -> put ((v, val) : env)
        Just _ -> let env' = [(vr, vl) | (vr, vl) <- env, vr /= v] in put ((v, val) : env')

lookupVar :: Ident -> Eval Value
lookupVar v = do
    env <- get
    case lookup v env of
        Nothing -> pure $ ErrorV ("unbound variable " ++ v)
        Just val -> pure val