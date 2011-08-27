{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    List Monad Transformer

------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types, FlexibleInstances #-}
module ListT where

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans

{------------------------------------------------------------------------------
    A direct implementation
        type ListT m a = m [a]
    would violate the monad laws, but we don't have that problem.
------------------------------------------------------------------------------}
data MPlus m a where
    MZero :: MPlus m a
    MPlus :: ListT m a -> ListT m a -> MPlus m a

type ListT m a = ProgramT (MPlus m) m a

    -- *sigh* I want to use type synonyms for type constructors, too;
    -- GHC doesn't accept  MonadMPlus (ListT m)
instance Monad m => MonadPlus (ProgramT (MPlus m) m) where
    mzero     = singleton MZero
    mplus m n = singleton (MPlus m n)

runListT :: Monad m => ListT m a -> m [a]
runListT = eval <=< viewT
    where
    eval :: Monad m => ProgramViewT (MPlus m) m a -> m [a]
    eval (Return x)         = return [x]
    eval (MZero     :>>= k) = return []
    eval (MPlus m n :>>= k) =
        liftM2 (++) (runListT (m >>= k)) (runListT (n >>= k))

testListT :: IO [()]
testListT = runListT $ do
    n <- choice [1..5]
    lift . print $ "You've chosen the number: " ++ show n
    where
    choice = foldr1 mplus . map return


    -- testing the monad laws, from the Haskellwiki
    -- http://www.haskell.org/haskellwiki/ListT_done_right#Order_of_printing
a,b,c :: ListT IO ()
[a,b,c] = map (lift . putChar) ['a','b','c']

    -- t1 and t2 have to print the same sequence of letters
t1 = runListT $ ((a `mplus` a) >> b) >> c
t2 = runListT $ (a `mplus` a) >> (b >> c)
