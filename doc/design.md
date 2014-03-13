This document discusses miscellaneous design decisions and remarks for the `operational` library. This is mainly so that I can still remember them in a couple of years.

Lifting control operations
--------------------------
The monad transformer `ProgramT` can automatically lift operations from the base monad, notably those from `MonadState` and `MonadIO`.

Until recently, I thought that this is restricted to algebraic operations and cannot be done for control operations. (For more on this nomenclature, see a [remark by Conor McBride][conor].) However, it turns that it can actually be done for some control operations as well!

  [conor]: http://www.haskell.org/pipermail/haskell-cafe/2010-April/076185.html

For instance, the `MonadReader` class has a control operation `local`. The point is that it is subject to the following laws

    local :: MonadReader r m => (r -> r) -> m a -> m a

    local r (lift   m) = lift (local r m)
    local r (return a) = return a
    local r (m >>=  k) = local r m >>= local r . k

Together with the requirement that the new instructions introduced by `ProgramT` do not interfere with the corresponding effect,

    local r (singleton instr) = singleton instr

these laws specify a unique lifting.

In other words, we can lift control operations whenever they obey laws that relate to `>>=` and `return`.

`mapMonad`
----------
Limestraël [has suggested][1] that the module `Control.Monad.Operational` includes a function

    mapMonad :: (Monad m, Monad n)
        => (forall a. m a -> n a) -> ProgramT instr m a -> ProgramT instr n a

which changes the base monad for the `ProgramT` monad transformer. A possible implementation is

    mapMonad f = id' <=< lift . f . viewT
        where
        id' (Return a) = return a
        id' (i :>>= k) = singleton i >>= mapMonad f . k

However, for the time being, I have [opted against][1] adding this function because there is no guarantee that the mapping function `forall. m a -> n a` actually preserves the argument.


  [1]: http://www.haskell.org/pipermail/haskell-cafe/2010-May/077094.html
  [2]: http://www.haskell.org/pipermail/haskell-cafe/2010-May/077097.html


Recursive type definitions with `Program`
-----------------------------------------
In the [unimo paper][unimo], the instructions carry an additional parameter that "unties" recursive type definition. For example, the instructions for `MonadPlus` are written

    data PlusI unimo a where
        Zero :: PlusI unimo a
        Plus :: unimo a -> unimo a -> PlusI unimo a

The type constructor variable `unimo` will be tied to `Unimo PlusI`.

In this library, I have opted for the conceptually simpler approach that requires the user to tie the recursion himself

    data PlusI a where
        Zero :: PlusI a
        Plus :: Program PlusI a -> Program PlusI a -> Plus I a

I am not sure whether this has major consequences for composeablity; at the moment I believe that the former style can always be recovered from an implementation in the latter style.


  [unimo]: http://web.cecs.pdx.edu/~cklin/papers/unimo-143.pdf "Chuan-kai Lin. Programming Monads Operationally with Unimo."
