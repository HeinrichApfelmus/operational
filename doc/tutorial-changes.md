
  [tutorial]: http://apfelmus.nfshost.com/articles/operational-monad.html

The `operational` library is based on ["The Operational Monad Tutorial"][tutorial], but features a slightly different API and implementation.

This document describes how the library has been changed compared to the tutorial.


Changes to the `Program` type
-----------------------------
For efficiency reasons, the type `Program` representing a list of instructions is now *abstract*. A function `view` is used to inspect the first instruction, it returns a type

    data ProgramView instr a where
        Return :: a -> ProgramView instr a
        (:>>=) :: instr a -> (a -> Program instr b) -> ProgramView instr b

which is much like the old `Program` type, except that `Then` was renamed to `:>>=` and that the subsequent instructions stored in the second argument of `:>>=` are stored in the type `Program`, not `ProgramView`.
 
To see an example of the new style, here the interpreter for the stack machine from the tutorial:

    interpret :: StackProgram a -> (Stack Int -> a)
    interpret = eval . view
        where
        eval :: ProgramView StackInstruction a -> (Stack Int -> a)
        eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
        eval (Pop    :>>= is) (a:stack) = interpret (is a ) stack
        eval (Return a)       stack     = a

So-called "view functions" like `view` are a common way of inspecting data structures that have been made abstract for reasons of efficiency; see for example `viewL` and `viewR` in [`Data.Sequence`][containers].

  [containers]: http://hackage.haskell.org/package/containers

Efficiency
----------
Compared to the original type from the tutorial, `Program` now supports `>>=` in O(1) time in most use cases. This means that left-biased nesting like

    let
        nestLeft :: Int -> StackProgram Int
        nestLeft 0 = return 0
        nestLeft n = nestLeft (n-1) >>= push
    in
        interpret (nestLeft n) []
       
will now take O(n) time. In contrast, the old `Program` type from the tutorial would have taken O(n^2) time, similar to `++` for lists taking quadratic time in when nested to the left.

However, this does *not* hold in a *persistent* setting. In particular, the example

    let
        p  = nestLeft n
        v1 = view p
        v2 = view p
        v3 = view p
    in
        v1 `seq` v2 `seq` v3

will take O(n) time for each call of `view` instead of O(n) the first time and O(1) for the other calls. But since monads are usually used ephemerally, this is much less a restriction than it would be for lists and `++`.

Monad Transformers
------------------
Furthermore, `Program` is actually a type synonym and expressed in terms of a monad transformer `ProgramT`

    type Program instr a = ProgramT instr Identity a

Likewise, `view` is a specialization of `viewT` to the identity monad. This change is transparent (except for error messages on type errors) for users who are happy with just `Program` but very convenient for those users who want to use it as a monad transformer.

The key point about the transformer version `ProgramT` is that in addition to the monad laws, it automatically satisfies the lifting laws for monad transformers as well

    lift . return        =  return
    lift m >>= lift . g  =  lift (m >>= g)

The corresponding view function `viewT` now returns the type `m (ViewT instr m a)`. It's not immediately apparent why this return type will do, but it's straightforward to work with, like in the following implementation of the list monad transformer:

    data PlusI m a where
        Zero :: PlusI m a
        Plus :: ListT m a -> ListT m a -> PlusI m a
    
    type ListT m a = ProgramT (PlusI m) m a
    
    runList :: Monad m => ListT m a -> m [a]
    runList = eval <=< viewT
        where
        eval :: Monad m => ProgramViewT (PlusI m) m a -> m [a]
        eval (Return x)        = return [x]
        eval (Zero     :>>= k) = return []
        eval (Plus m n :>>= k) =
            liftM2 (++) (runList (m >>= k)) (runList (n >>= k))



Alternatives to Monad Transformers
----------------------------------
By the way, note that monad transformers are not the only way to build larger monads from smaller ones; a similar effect can be achieved with the direct sum of instructions sets. For instance, the monad

    Program (StateI s :+: ExceptionI e) a

    data (f :+: g) a = Inl (f a) | Inr (g a)  -- a fancy  Either

is a combination of the state monad

    type State a = Program (StateI s) a

    data StateI s a where
        Put :: s -> StateI s ()
        Get :: StateI s s

and the error monad

    type Error e a = Program (ErrorI e) a

    data ErrorI e a where
        Throw :: e -> ErrorI e ()
        Catch :: ErrorI e a -> (e -> ErrorI e a) -> ErrorI e a

The "sum of signatures" approach and the `(:+:)` type constructor are advocated in [Wouter Swierstra's "Data Types a la carte"][a la carte]. Time will tell which has more merit; for now I have opted for a seamless interaction with monad transformers.

  [a la carte]: http://www.cse.chalmers.se/~wouter/Publications/DataTypesALaCarte.pdf "Wouter Swierstra. Data types à la carte."
