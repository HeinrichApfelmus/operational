{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    State monad and monad transformer
    
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types, FlexibleInstances #-}
module State where

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans

{------------------------------------------------------------------------------
	State Monad
------------------------------------------------------------------------------}
data StateI s a where
    Get :: StateI s s
    Put :: s -> StateI s ()

type State s a = Program (StateI s) a

evalState :: State s a -> s -> a
evalState = eval . view
    where
    eval :: ProgramView (StateI s) a -> (s -> a)
    eval (Return x)     = const x
    eval (Get   :>>= k) = \s -> evalState (k s ) s
    eval (Put s :>>= k) = \_ -> evalState (k ()) s

put :: s -> StateT s m ()
put = singleton . Put

get :: StateT s m s
get = singleton Get

testState :: Int -> Int
testState = evalState $ do
        x <- get
        put (x+2)
        get

{------------------------------------------------------------------------------
    State Monad Transformer
------------------------------------------------------------------------------}
type StateT s m a = ProgramT (StateI s) m a

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = \s -> viewT m >>= \p -> eval p s
    where
    eval :: Monad m => ProgramViewT (StateI s) m a -> (s -> m a)
    eval (Return x)     = \_ -> return x
    eval (Get   :>>= k) = \s -> evalStateT (k s ) s
    eval (Put s :>>= k) = \_ -> evalStateT (k ()) s

testStateT = evalStateT $ do
    x <- get
    lift $ putStrLn "Hello StateT"
    put (x+1)
