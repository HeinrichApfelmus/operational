This document discussion miscellaneous design decisions for the `operational` library.

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
