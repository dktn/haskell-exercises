{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- more info in:
-- http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf

module ExpressionEval where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Map (Map)
import qualified Data.Map as Map

type Name   =  String                -- variable names

data Exp    =  Lit  Integer          -- expressions
            |  Var  Name
            |  Plus Exp  Exp
            |  Abs  Name Exp
            |  App  Exp  Exp
            deriving (Eq, Show)

data Value  =  IntVal Integer        -- values
            |  FunVal Env Name Exp
            deriving (Eq, Show)

type Env    =  Map Name Value        -- from names to values

type EvalState = Integer

type EvalLog   = String

type EvalError = String

--------------------------------------------------------------------------------------------------------

tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

--------------------------------------------------------------------------------------------------------

type Eval alpha = ReaderT Env (ExceptT EvalError (WriterT [EvalLog] (StateT EvalState IO))) alpha

runEval :: Env -> Integer -> Eval alpha -> IO ((Either String alpha, [String]), Integer)
runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

runExp :: Exp -> IO ((Either String Value, [String]), Integer)
runExp exp = runEval Map.empty 0 $ eval exp

type MonadEval m = (MonadState EvalState m, MonadWriter [EvalLog] m, MonadReader Env m, MonadError EvalError m, MonadIO m)

eval :: MonadEval m => Exp -> m Value
eval (Lit i) = do
    tick
    liftIO $ putStrLn $ "Literal " <> show i -- print each int when evaluated
    return $ IntVal i
eval (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " ++ n)
        Just val -> return val
eval (Plus e1 e2) = do
    tick
    e1' <- eval e1
    e2' <- eval e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
        _                      -> throwError "type error in addition"
eval (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval (App  e1 e2) = do
    tick
    val1 <- eval e1
    val2 <- eval e2
    case val1 of
        FunVal env' n body -> local (const $ Map.insert n val2 env') $ eval body
        _                  -> throwError "type error in application"


--------------------------------------------------------------------------------------------------------

-- 12 + (\x -> x) (4 + 2)
exampleExp1 :: Exp
exampleExp1 = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))

exampleExp2 :: Exp
exampleExp2 = App (Abs "x" (Plus (Var "x") (Var "x"))) (Lit 7)

-- TASKS:

-- 1)
-- run:
-- > runExp exampleExp1
-- interpret the output


-- 2)
-- Beside the variables add to log the expressions being currently evaluated
