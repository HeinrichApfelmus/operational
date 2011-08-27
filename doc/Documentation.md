% Documentation for the "operational" package
% Heinrich Apfelmus
% Sun, 18 Apr 2010 13:06:16 +0200

  [tutorial]: http://themonadreader.wordpress.com/2010/01/26/issue-15/
  [unimo]: http://web.cecs.pdx.edu/~cklin/papers/unimo-143.pdf "Chuan-kai Lin. Programming Monads Operationally with Unimo."
  [hughes]: http://citeseer.ist.psu.edu/hughes95design.html "John Hughes. The Design of a Pretty-printing Library."
  [prompt]: http://hackage.haskell.org/package/MonadPrompt "Ryan Ingram's Monad Prompt Package."

<!-- *The HTML version of this document is generated automatically from the corresponding markdown file, don't change it!* -->

Introduction
============
This package is based on ["The Operational Monad Tutorial"][tutorial] and this documentation describes its extension to a production-quality library. In other words, the "magic" gap between relevant paper and library implementation is documented here.

Take note that this this library is only ~50 lines of code, yet the documentation even includes a proof! :-)

Sources and inspiration for this library include [Chuan-kai Lin's unimo paper][unimo], [John Hughes 95][hughes], and [Ryan Ingram's `MonadPrompt` package][prompt].

Using this Library
=================
To understand what's going on, you'll have to read ["The Operational Monad Tutorial"][tutorial]. Here, I will first and foremost note the changes with respect to the tutorial.

Several advanced [example monads](./examples.html) demonstrate how to put this library to good use. In the source distribution, the corresponding source files can also be found in the `.docs/examples` folder.

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

  [containers]: http://hackage.haskell.org/package/containers-0.3.0.0

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

  [a la carte]: http://www.cse.chalmers.se/~wouter/Publications/DataTypesALaCarte.pdf "Wouter Swierstra. Data types ˆ la carte."


Design and Implementation
=========================
Proof of the monad laws (Sketch)
--------------------------------
The key point of this library is of course that the `view` and `viewT` functions respect the monad laws. While this seems obvious from the definition, the proof is actually not straightforward.

First, we restrict ourselves to `view`, i.e. the version without monad transformers. In fact, I don't have a full proof for the version with monad transformers, more about that in the next section.

Second, we use a sloppy, but much more suitable notation, namely we write

---------    -------------------------
`>>=`         instead of `Bind`
`return`      instead of `Lift` for the identity monad
`i,j,k,`...   for primitive instructions
-------------------------------------------------------------

Then, the `view` function becomes

    view (return a)        = Return a
    view (return a  >>= g) = g a                           -- left unit
    view ((m >>= f) >>= g) = view (m >>= (\x -> f x >>= g) -- associativity
    view (i         >>= g) = i :>>= g
    view  i                = i :>>= return                 -- right unit

Clearly, `view` uses the monad laws to rewrite it's argument. But we want to show that whenever two expressions

    e1,e2 :: Program instr a

can be transformed into each other by rewriting them with the monad laws in *any* fashion (remember that `>>=` and `return` are constructors), then `view` will map them to the same result. More formally, we have an equivalence relation

    e1 ~ e2   iff   e1 and e2 are the same modulo monad laws

and want to show

    e1 ~ e2  =>   view e1 = view e2    (some notion of equality)

Now, this needs proof because `view` is like a term rewriting system and there is no guarantee that two equivalent terms will be rewritten to the same normal form.

Trying to attack this problem with term rewriting and critical pairs is probably hopeless and not very enlightening. After all, the theorem should be obvious because two equivalent expressions should have the same *first instruction* `i`. Well, we can formalize this with the help of a *normal form*

    data NF instr a where
        Return' :: a -> NF instr a
        (:>>=') :: instr a -> (a -> NF instr b) -> NF instr b

This is the old program type and the key observation is that `NF instr` is already a monad.

    instance Monad (NF inst) where
        (Return' a) >>= g = g a
        (m :>>=' f) >>= g = m :>>= (\x -> f x >>= g)

(I'll skip the short calculation and coinduction argument that this really fulfills the monad laws.) We can define a normalization function

    normalize :: Program instr a -> NF instr a
    normalize (m >>= g)  = normalize m >>=' normalize g
    normalize (return a) = Return' a
    normalize  i         = i :>>=' Return'

which has the now obvious property that

    e1 ~ e2  =>  normalize e1 = normalize e2

Now, the return type of `view` is akin to a *head normal form*, hence

       normalize (view e1) = normalize (view e2) 
    => view e1 = view e2

(for some suitable extension of `normalize` to the `ProgramView` type.) But since `view` only uses monad laws to rewrite its argument, we also have

    e1 ~ view e1  =>  normalize e1 = normalize (view e1)

and this concludes the proof, which pretty much only showed that two equivalent expressions have the same instruction list and hence `view` gives equal results.

Monad Transformers
------------------
The monad transformer case is more hairy, I have no proof here. (If you read this by accident: don't worry, it's still correct. This is for proof nerds only.)

The main difficulty is that the equation

    return = lift . return

is an equation for the already existing `return` constructor and the notion of "first instruction" no longer applies. Namely, we have

    m  =  return m >>= id  =  lift (return m) >>= id

and it's not longer clear what a suitable normal form might be. It appears that `viewT` rewrites the term as follows

      lift m >>= g
    = lift m >>= (\x -> lift (return (g x)) >>= id)
    = (lift m >>= lift . return . g) >>= id
    = lift (m >>= return . g) >>= id

(To be continued.)


Other Design Choices
====================
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

