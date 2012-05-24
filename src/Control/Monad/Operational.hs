{-# LANGUAGE GADTs, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
-- Search for UndecidableInstances to see why this is needed

{-| Implement monads by specifying primitive instructions and their operational semantics.

This package is based on the \"The Operational Monad Tutorial\", published in Issue 15 of The Monad.Reader <http://themonadreader.wordpress.com/>.

You are reading the API reference. For more thorough documentation including design and implementation notes as well as a correctness proof, please consult the included documentation in @doc\/Documentation.md@, also available at <http://heinrichapfelmus.github.com/operational/Documentation.html> .

This API reference includes only basic example code. More intricate examples are available in the @doc\/examples@ directory, also available at <https://github.com/HeinrichApfelmus/operational/tree/master/doc/examples#readme>.
-}
module Control.Monad.Operational (
    -- * Basic usage
    Program, singleton, ProgramView, view,
    -- $example
    
    -- * Monad transformer
    ProgramT, ProgramViewT(..), viewT, liftProgram
    -- $exampleT
    
    ) where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Applicative

    -- mtl  classes to instantiate.
    -- Those commented out cannot be instantiated. For reasons see below.
-- import Control.Monad.Cont.Class
-- import Control.Monad.Error.Class
-- import Control.Monad.Reader.Class
import Control.Monad.State.Class
-- import Control.Monad.Writer.Class

{------------------------------------------------------------------------------
   Program
------------------------------------------------------------------------------}
{-| The abstract data type 'Program instr a' represents programs.

    * The type constructor @instr :: * -> *@ indexes the primitive instructions.
    
    * @a@ is the return type of a program.
    
    @'Program' instr@ is always a monad and
    automatically obeys the monad laws.
-}
type Program instr = ProgramT instr Identity

-- | View type for inspecting the first instruction.
--   It has two constructors 'Return' and @:>>=@.
--   (For technical reasons, they are documented at 'ProgramViewT'.)
type ProgramView instr  = ProgramViewT instr Identity

-- | View function for inspecting the first instruction.
view :: Program instr a -> ProgramView instr a
view = runIdentity . viewT

{- $example

/Example usage/

Stack machine from \"The Operational Monad Tutorial\".

@
    data StackInstruction a where
        Push :: Int -> StackInstruction ()
        Pop  :: StackInstruction Int
@

@
    type StackProgram a = Program StackInstruction a
@

@
    interpret :: StackProgram a -> (Stack Int -> a)
    interpret = eval . view
        where
        eval :: ProgramView StackInstruction a -> (Stack Int -> a)
        eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
        eval (Pop    :>>= is) (a:stack) = interpret (is a ) stack
        eval (Return a)       stack     = a
@

Note that since 'ProgramView' is a GADT, the type annotation for @eval@ is mandatory.

-}

{------------------------------------------------------------------------------
    ProgramT - monad transformer
------------------------------------------------------------------------------}
{-| The abstract data type @'ProgramT' instr m a@ represents programs.

    * The type constructor @instr :: * -> *@ indexes the primitive instructions.
    
    * @m@ is the base monad, embedded with 'lift'.

    * @a@ is the return type of a program.
    
    @'ProgramT' instr m@ is a monad transformer and
    automatically obeys both the monad and the lifting laws.
-}
data ProgramT instr m a where
    Lift   :: m a -> ProgramT instr m a
    Bind   :: ProgramT instr m b -> (b -> ProgramT instr m a)
           -> ProgramT instr m a
    Instr  :: instr a -> ProgramT instr m a

    -- basic instances
instance Monad m => Monad (ProgramT instr m) where
    return = Lift . return
    (>>=)  = Bind

instance MonadTrans (ProgramT instr) where
    lift   = Lift

instance Monad m => Functor (ProgramT instr m) where
    fmap   = liftM

instance Monad m => Applicative (ProgramT instr m) where
    pure   = return
    (<*>)  = ap

-- | Program made from a single primitive instruction.
singleton :: instr a -> ProgramT instr m a
singleton = Instr

-- | View type for inspecting the first instruction.
data ProgramViewT instr m a where
    Return :: a -> ProgramViewT instr m a
    (:>>=) :: instr b -> (b -> ProgramT instr m a ) -> ProgramViewT instr m a

-- | View function for inspecting the first instruction.
viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
viewT (Lift m)                = m >>= return . Return
viewT ((Lift m)     `Bind` g) = m >>= viewT . g
viewT ((m `Bind` g) `Bind` h) = viewT (m `Bind` (\x -> g x `Bind` h))
viewT ((Instr i)    `Bind` g) = return (i :>>= g)
viewT (Instr i)               = return (i :>>= return)

{-| Lift a plain sequence of instructions to a sequence
    of instructions over a monad 'm'.
    This is the counterpart of the 'lift' function from 'MonadTrans'.
    
    It can be defined as follows:

@
    liftProgram = eval . view
        where
        eval :: ProgramView instr a -> ProgramT instr m a
        eval (Return a) = return a
        eval (i :>>= k) = singleton i >>= liftProgram . k
@
    
-}
liftProgram :: Monad m => Program instr a -> ProgramT instr m a
liftProgram (Lift m)     = return (runIdentity m)
liftProgram (m `Bind` k) = liftProgram m `Bind` (liftProgram . k)
liftProgram (Instr i)    = Instr i

{- $exampleT

/Example usage/

List monad transformer.

@
    data PlusI m a where
        Zero :: PlusI m a
        Plus :: ListT m a -> ListT m a -> PlusI m a
@

@   
    type ListT m a = ProgramT (PlusI m) m a
@

@   
    runList :: Monad m => ListT m a -> m [a]
    runList = eval <=< viewT
        where
        eval :: Monad m => ProgramViewT (PlusI m) m a -> m [a]
        eval (Return x)        = return [x]
        eval (Zero     :>>= k) = return []
        eval (Plus m n :>>= k) =
            liftM2 (++) (runList (m >>= k)) (runList (n >>= k))
@

Note that since 'ProgramView' is a GADT, the type annotation for @eval@ is mandatory.

-}

{------------------------------------------------------------------------------
    mtl instances
    
  * All of these instances need UndecidableInstances,
    because they do not satisfy the coverage condition.
    
  * We can only make instances of those classes
    that do not contain control operators. Control operators are
    functions like
    
        listen :: m a -> m (a,w)
    
    that take arguments of type  m a  and cannot be implemented with  lift .
    
    See also Conor McBride's remark on haskell-cafe:
    http://www.haskell.org/pipermail/haskell-cafe/2010-April/076185.html
------------------------------------------------------------------------------}
instance (MonadState s m) => MonadState s (ProgramT instr m) where
    get = lift get
    put = lift . put

instance (MonadIO m) => MonadIO (ProgramT instr m) where
    liftIO = lift . liftIO

{- Attempt to lift control operators anyway. WARNING: HERE BE DRAGONS

    -- lift a control operation with a single argument
    -- Unfortunately, I don't have a specifications in terms of laws yet
    -- I think it makes use of
    --   liftControlOp1 f (lift m >>= k) = lift m >>= liftControlOp1 f . k
    -- and is therefore completely useless.
liftControlOp1 :: Monad m =>
    (m a -> m b) -> (ProgramT instr m a -> ProgramT instr m b)
liftControlOp1 f = join . lift . (eval <=< viewT)
    where
    eval :: ProgramViewT instr m a -> m (ProgramT instr m a)
    eval (Return a) = f (return a)
    eval (i :>>= k) = i :>>= liftControlOp1 f . k

instance (MonadWriter w m) => MonadWriter w (ProgramT instr m) where
    tell   = lift . tell
    listen = liftControlOp1 listen
    pass   = liftControlOp1 pass
-}
