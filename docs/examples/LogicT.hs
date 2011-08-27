{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    Oleg's  LogicT  monad transformer
    
    Functions to implement are taken from the corresponding paper
    http://okmij.org/ftp/papers/LogicT.pdf
    
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types #-}
module LogicT (LogicT, msplit, observe, bagOfN, interleave) where

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans

import Data.Maybe

{------------------------------------------------------------------------------
    LogicT
    = A MonadPlus with an additional operation
         msplit
      which returns the first result and a computation to 
      produce the remaining results.


    For example, the function  msplit  satisfies the laws
    
        msplit mzero                 ~> return Nothing
        msplit (return a `mplus` m)  ~> return (Just (a,m))
    
    It turns out that we don't have to make  msplit  a primitive,
    we can implement it by inspection on the argument. In other
    words,  LogicT  will be the same as the  ListT  monad transformer
------------------------------------------------------------------------------}
import ListT
type LogicT m a = ListT m a

    -- msplit  is the lift of a function  split  in the base monad
msplit :: Monad m => LogicT m a -> LogicT m (Maybe (a, LogicT m a))
msplit = lift . split

    -- split  in the base monad
split :: Monad m => LogicT m a -> m (Maybe (a, LogicT m a))
split = eval <=< viewT
    where
    -- apply the laws for  msplit
    eval :: Monad m => ProgramViewT (MPlus m) m a -> m (Maybe (a, LogicT m a))
    eval (MZero     :>>= k) = return Nothing
    eval (MPlus m n :>>= k) = do
        ma <- split (m >>= k)
        case ma of
            Nothing     -> split (n >>= k)
            Just (a,m') -> return $ Just (a, m' `mplus` (n >>= k))
                                --            inefficient!
                                -- `mplus` will add another (>>= return)
                                -- to  n  each time it's called.
                                -- Curing this is not easy.

    -- main interpreter, section 6 in the paper
    -- returns the first result, if any; may fail
observe :: Monad m => LogicT m a -> m a
observe m = (fst . fromJust) `liftM` split m

{------------------------------------------------------------------------------
    Derived functions from the paper
------------------------------------------------------------------------------}
    -- return the first n results, section 6
bagOfN :: Monad m => Maybe Int -> LogicT m a -> LogicT m [a]
bagOfN (Just n) m | n <= 0 = return []
bagOfN n m                 = msplit m >>= bagofN'
    where
    bagofN' Nothing         = return []
    bagofN' (Just (x,m'))   = (x:) `liftM` bagOfN (fmap pred  n) m'
                            where pred n = n-1

    -- interleave
interleave :: Monad m => LogicT m a -> LogicT m a -> LogicT m a
interleave m1 m2 = do
    r <- msplit m1
    case r of
        Nothing      -> m2
        Just (a,m1') -> return a `mplus` interleave m2 m1'


