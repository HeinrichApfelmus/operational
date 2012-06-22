{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    Koen Claessen's Poor Man's Concurrency Monad
    http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.39.8039

------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types #-}
module PoorMansConcurrency where

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans hiding (lift)

{------------------------------------------------------------------------------
    A concurrency monad runs several processes in parallel
    and supports two primitive operations

        fork  -- fork a new process
        stop  -- halt the current one
    
    We want this to be a monad transformer, so we also need a function  lift
    This time, however, we cannot use the monad transformer version  ProgramT
    because this will leave no room for interleaving different computations
    of the base monad.
------------------------------------------------------------------------------}
data ProcessI m a where
    Lift :: m a -> ProcessI m a
    Stop :: ProcessI m a
    Fork :: Process m () -> ProcessI m ()


type Process m a = Program (ProcessI m) a

stop = singleton Stop
fork = singleton . Fork
lift = singleton . Lift

-- interpreter
runProcess :: Monad m => Process m a -> m ()
runProcess m = schedule [m]
    where
    schedule (x:xs) = run (view x) xs

    run :: Monad m => ProgramView (ProcessI m) a -> [Process m a] -> m ()
    run (Return _)      xs = return ()                 -- process finished
    run (Lift m :>>= k) xs = m >>= \a ->               -- switch process
                             schedule (xs ++ [k a])
    run (Stop   :>>= k) xs = schedule xs               -- process halts
    run (Fork p :>>= k) xs = schedule (xs ++ [x2,x1])  -- fork new process
        where x1 = k (); x2 = p >>= k

-- example
--      > runProcess example   -- warning: runs indefinitely
example :: Process IO ()
example = do
        write "Start!"
        fork (loop "fish")
        loop "cat"

write  = lift . putStr
loop s = write s >> loop s
