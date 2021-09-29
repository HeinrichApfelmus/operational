{-# LANGUAGE GADTSyntax, ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
-- Search for UndecidableInstances to see why this is needed

module Control.Monad.Operational (
    -- * Synopsis
    -- $synopsis

    -- * Overview
    -- $intro

    -- * Monad
    Program, singleton, ProgramView, view,
    -- $example
    interpretWithMonad,

    -- * Monad transformer
    ProgramT, ProgramViewT(..), viewT,
    -- $exampleT
    liftProgram, mapInstr,
    unviewT, interpretWithMonadT,

    ) where

import Control.Monad.Identity
import Control.Monad.Trans

    -- mtl  classes to instantiate.
    -- Those commented out cannot be instantiated. For reasons see below.
-- import Control.Monad.Cont.Class
-- import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
-- import Control.Monad.Writer.Class

{------------------------------------------------------------------------------
    Introduction
------------------------------------------------------------------------------}
{-$synopsis
To write a monad, use the 'Program' type.

To write a monad transformer, use the 'ProgramT' type.

For easier interoperability,
the 'Program' type is actually a type synonym
and defined in terms of 'ProgramT'.
-}

{-$intro

The basic idea for implementing monads with this libary
is to think of monads as /sequences of primitive instructions/.
For instance, imagine that you want to write a web application
with a custom monad that features an instruction

> askUserInput :: CustomMonad UserInput

which sends a form to the remote user and waits for the user
to send back his input

To implement this monad, you decide that this instruction is
a primitive, i.e. should not be implemented in terms of other,
more basic instructions.
Once you have chosen your primitives, collect them in a data type

@
data CustomMonadInstruction a where
    AskUserInput :: CustomMonadInstruction UserInput
@

Then, obtain your custom monad simply by applying the 'Program'
type constructor

> type CustomMonad a = Program CustomMonadInstruction a

The library makes sure that it is an instance of the 'Monad' class
and fulfills all the required laws.

Essentially, the monad you now obtained is just a
fancy list of primitive instructions.
In particular, you can pattern match on the first element of this "list".
This is how you implement an @interpret@ or @run@ function for your monad.
Note that pattern matching is done using the 'view' function

@
runCustomMonad :: CustomMonad a -> IO a
runCustomMonad m = case view m of
    Return a            -> return a -- done, return the result
    AskUserInput :>>= k -> do
        b <- waitForUserInput       -- wait for external user input
        runCustomMonad (k b)        -- proceed with next instruction
@

The point is that you can now proceed in any way you like:
you can wait for the user to return input as shown,
or you store the continuation @k@ and retrieve it when
your web application receives another HTTP request,
or you can keep a log of all user inputs on the client side and replay them,
and so on. Moreover, you can implement different @run@ functions
for one and the same custom monad, which is useful for testing.
Also note that the result type of the @run@ function does not need to
be a monad at all.

In essence, your custom monad allows you to express
your web application as a simple imperative program,
while the underlying implementation can freely map this to
an event-drived model or some other control flow architecture
of your choice.

The possibilities are endless.
More usage examples can be found here:
<https://github.com/HeinrichApfelmus/operational/tree/master/doc/examples#readme>

-}

{------------------------------------------------------------------------------
   Program
------------------------------------------------------------------------------}
{-| The abstract data type @'Program' instr a@ represents programs,
    i.e. sequences of primitive instructions.

    * The /primitive instructions/ are given by the type constructor @instr :: * -> *@.

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


-- | Utility function that extends
-- a given interpretation of instructions as monadic actions
-- to an interpration of 'Program's as monadic actions.
--
-- This function can be useful if you are mainly interested in
-- mapping a 'Program' to different standard monads, like the state monad.
-- For implementing a truly custom monad,
-- you should write your interpreter directly with 'view' instead.
interpretWithMonad :: forall instr m b.
    Monad m => (forall a. instr a -> m a) -> (Program instr b -> m b)
interpretWithMonad f = eval . view
    where
    eval :: forall a. ProgramView instr a -> m a
    eval (Return a) = return a
    eval (m :>>= k) = f m >>= interpretWithMonad f . k

{- $example

/Example usage/

Stack machine from \"The Operational Monad Tutorial\".

>    data StackInstruction a where
>        Push :: Int -> StackInstruction ()
>        Pop  :: StackInstruction Int
>
>    type StackProgram a = Program StackInstruction a
>    type Stack b        = [b]
>
>    interpret :: StackProgram a -> (Stack Int -> a)
>    interpret = eval . view
>        where
>        eval :: ProgramView StackInstruction a -> (Stack Int -> a)
>        eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
>        eval (Pop    :>>= is) (a:stack) = interpret (is a ) stack
>        eval (Return a)       stack     = a

In this example, the type signature for the `eval` helper function is optional.

-}

{------------------------------------------------------------------------------
    ProgramT - monad transformer
------------------------------------------------------------------------------}
{-| The abstract data type @'ProgramT' instr m a@ represents programs
    over a base monad @m@,
    i.e. sequences of primitive instructions and actions from the base monad.

    * The /primitive instructions/ are given by the type constructor @instr :: * -> *@.

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
-- This is very similar to pattern matching on lists.
--
-- * The case @(Return a)@ means that the program contains no instructions
-- and just returns the result @a@.
--
-- *The case @(someInstruction :>>= k)@ means that the first instruction
-- is @someInstruction@ and the remaining program is given by the function @k@.
data ProgramViewT instr m a where
    Return :: a -> ProgramViewT instr m a
    (:>>=) :: instr b
           -> (b -> ProgramT instr m a)
           -> ProgramViewT instr m a

instance Monad m => Functor (ProgramViewT instr m) where
    fmap f (Return a) = Return $ f a
    fmap f (instr :>>= cont) = instr :>>= (fmap f . cont)

instance Monad m => Applicative (ProgramViewT instr m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ProgramViewT instr m) where
    return = Return
    Return a >>= cont = cont a
    (instr :>>= cont1) >>= cont2 = instr :>>= (cont1 >=> unviewT . cont2)

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


-- | Utility function that extends
-- a given interpretation of instructions as monadic actions
-- to an interpration of 'ProgramT's as monadic actions.
--
-- Ideally, you would not use another monad,
-- but write a custom interpreter directly with `viewT`.
-- See the remark at 'interpretWithMonad'.
interpretWithMonadT :: Monad m => (forall x . instr x -> m x) -> ProgramT instr m a -> m a
interpretWithMonadT interpreter = go
  where
    go program = do
      firstInstruction <- viewT program
      case firstInstruction of
        Return a -> return a
        instruction :>>= continuation -> interpreter instruction >>= (go . continuation)

-- | Utilitiy function for mapping a 'ProgramViewT' back into a 'ProgramT'.
-- 
-- Semantically, the function 'unviewT' is an inverse of 'viewT',
-- e.g. we have
--
-- @
--   viewT (singleton i) >>= unviewT = return (singleton i)
-- @
unviewT :: Monad m => ProgramViewT instr m a -> ProgramT instr m a
unviewT (Return a) = return a
unviewT (instruction :>>= continuation) =
    (Instr instruction) `Bind` continuation

-- | Extend a mapping of instructions to a mapping of 'ProgramT'.
mapInstr ::
    forall instr1 instr2 m a . Monad m
    => (forall x . instr1 x -> instr2 x)
    -> ProgramT instr1 m a -> ProgramT instr2 m a
mapInstr f = go
    where
        go :: forall x. ProgramT instr1 m x -> ProgramT instr2 m x
        go (Lift action) = Lift action
        go (Bind action continuation) = Bind (go action) (go . continuation)
        go (Instr instruction) = Instr $ f instruction

{- $exampleT

/Example usage/

List monad transformer.

>    data PlusI m a where
>        Zero :: PlusI m a
>        Plus :: ListT m a -> ListT m a -> PlusI m a
>
>    type ListT m a = ProgramT (PlusI m) m a
>
>    runList :: Monad m => ListT m a -> m [a]
>    runList = eval <=< viewT
>        where
>        eval :: Monad m => ProgramViewT (PlusI m) m a -> m [a]
>        eval (Return x)        = return [x]
>        eval (Zero     :>>= k) = return []
>        eval (Plus m n :>>= k) =
>            liftM2 (++) (runList (m >>= k)) (runList (n >>= k))

In this example, the type signature for the `eval` helper function is optional.

-}

{------------------------------------------------------------------------------
    mtl instances

  * All of these instances need UndecidableInstances,
    because they do not satisfy the coverage condition.
    Most of the instance in the  mtl  package itself have the same issue.

  * Lifting algebraic operations is easy,
    lifting control operations is more elaborate, but sometimes possible.
    See the design notes in  `doc/design.md`.
------------------------------------------------------------------------------}
instance (MonadState s m) => MonadState s (ProgramT instr m) where
    get = lift get
    put = lift . put

instance (MonadIO m) => MonadIO (ProgramT instr m) where
    liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (ProgramT instr m) where
    ask = lift ask

    local r (Lift m)     = Lift (local r m)
    local r (m `Bind` k) = local r m `Bind` (local r . k)
    local _ (Instr i)    = Instr i
