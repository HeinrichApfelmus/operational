This document discusses miscellaneous design decisions for the `operational` library. This is mainly so that I can still remember them in a couple of years.


`mapMonad`
----------

Limestraël [has suggested][1] that the module `Control.Monad.Operational` includes a function

    mapMonad :: (Monad m, Monad n)
        => (forall a. m a -> n a) -> ProgramT instr m a -> ProgramtT instr n a

which changes the base monad for the `ProgramT` monad transformer. A possible implementation is

    mapMonad f = id' <=< lift . f . viewT
        where
        id' :: ProgramViewT instr m a -> ProgramT instr n a
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
