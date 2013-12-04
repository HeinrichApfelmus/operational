Correctness Proofs
==================

This document collects correctness proofs for the `operational` library.

  [tutorial]: http://apfelmus.nfshost.com/articles/operational-monad.html

Monad laws
----------

For reasons of efficiency, the `Program` type is not implemented as a list of instructions as presented in the [The Operational Monad Tutorial][tutorial]. However, this means that we now have to prove that the implementations of `view` and `viewT` *respect the monad laws*.

In particular, we say that two programs

    e1, e2 :: Program instr a

are *equivalent*, `e1 ~ e2`, if they can be transformed into each other by applying the monad laws on the constructors. For instance, the expressions

    e1 = ((m `Bind` f) `Bind` g
    e2 = m `Bind` (\a -> f a `Bind` g)

are equivalent for any expressions `m`, `f` and `g`. Our goal is to show that the `view` functions give the same result for equivalent expressions:

    e1 ~ e2  =>  view e1 = view e2

The `ProgramView` type is equipped with an appropriate equality relation:

    (Return a1)  = (Return a2)   iff   a1 = a2
    (i1 :>>= k1) = (i2 :>>= k2)  iff   i1 = i2  and  k1 x ~ k2 x  for all x

### Normal form: list of instructions

The key observation for the proof is the following: As in the [tutorial][], the `Program` type represents a list of instructions. The representation is redundant for the purpose of efficiency, but different expressions should still correspond to the same list of instructions if they are equivalent. After all, equivalence is just about the associativity of the `Bind` operation. This also means that the first instruction, and hence the result of `view` should be unique for each equivalence class.

For simplicity, let us first focus on the pure `Program` type and postpone the case `ProgramT` for monad transformers later.

We can formalize the intuition above by introducing the following types of *normal form*

    data NF instr a where
        Return' :: a -> NF instr a
        (:>>=') :: instr a -> (a -> NF instr b) -> NF instr b

which is simply the list of instructions from the [tutorial][]. Now, we know that `NF` is a monad

    instance Monad (NF instr) where
        return            = Return'
        (Return' a) >>= k = k a
        (m :>>=' g) >>= k = m :>>=' (\a -> g a :>>=' k)

In particular, it fulfills the monad laws. (Actually we would have to prove that by using coinduction, but I leave that as an exercise.)

We can now map each `Program` to its normal form

    normalize :: Program instr a -> NF instr a
    normalize (m `Bind` k) = normalize m >>= normalize k
    normalize (Return a)   = return a
    normalize (Instr  i)   = i :>>=' return

In particular, note that this function is a morphism and `NF` fulfills the monad laws. Hence, equivalent programs will be mapped to the same normal form, i.e.

    e1 ~ e2  =>  normalize e1 = normalize e2


How does this observation help us? Note that the `view` only uses the monad laws to rewrite a `Program`. Using a somewhat sloppy notation, we express this as

    e1 ~ view e1

where we intepret a view  `i :>>= k` as the "obvious" `Program` expression `Bind (Instr i) k` where the left argument of the `Bind` constructor is an instruction. Furthermore, we can think of the `ProgramView` type as a head normal form. In other words, applying `normalize` to an expression of the form `view e1` will not change the first instruction, which means

    normalize (view e1) = normalize (view e2)  =>  view e1 = view e2

(The requires a coinductive argument for the tail of instructions.)

Taking these three implications together, we see that

    e1 ~ e2  =>  view e1 = view e2

as desired.

### Normal form for monad transformers

A similar technique can be used to show that the monad laws also hold for the monad transformer version `ProgramT`. The key observation here is that the normal form is an *effectful list of instructions*

    newtype NFT instr m a = JoinLift (m (NFT' instr m a))

    data NFT' instr m a where
        Return' :: a -> NFT' instr m a
        (:>>=') :: instr a -> (a -> NFT instr m b) -> NFT' instr m b

This is in very close analogy to the "effectful list"

    type ListT  m a = m (ListT' m a)
    data ListT' m a = Nil | Cons a (ListT m a)

For example, if the monad `m` is the state monad, then this type represents a list whose tail depends on the current state.

First, we convince ourselves that the `NFT` type is indeed a monad transformer. The corresponding functions are implemented as

    instance Monad m => Monad (NFT instr m) where
        return a = JoinLift (return (Return' a))
    
        (JoinLift m) >>= k  = JoinLift (m >>= f)
            where
            f (Return' a) = k a
            f (i :>>=' f) = return $ i :>>= (\a -> f a >>= k)

    instance MonadTrans (NFT instr) where
        lift m = JoinLift (fmap Return' m)
    
    singleton i = JoinLift (return (i :>>=' return))

It is somewhat tedious to check the monad laws and the lifting laws, so we skip this step here.


Having convinced ourselves that the normal form type `NFT` is, in fact, a monad transformer, we can define a morphism

    normalize :: ProgramT instr m a -> NFT instr m a
    normalize (Lift m)     = lift m
    normalize (m `Bind` k) = m >>= k
    normalize (Instr i)    = singleton i

and obtain that equivalent programs are mapped to equal normal forms. Similar to the pure case, normalizing the result of `viewT` will not change the first instruction, and we can conclude that the result `viewT` only depends on the normal form of the argument.


Lifting monads
--------------

The normal forms are also useful for proving that class instances can be lifted from a base monad `m` to the monad `ProgramT instr m`.

### Instructions and control operations

Some monads only feature "algebraic" instructions which have the form

    instr :: a1 -> a2 -> ... -> m b

so that the types `a1`, `a2`, etc. of the parameters do not contain the monad `m` again. For example, the state monad has two instructions

    get :: State s s
    put :: s -> State s ()

of precisely this form. Lifting these kinds of instructions is straightforward, i.e. the `ProgramT instr State` monad is also a state monad.

    instance (MonadState s m) => MonadState s (ProgramT instr m) where
        get = lift get
        put = lift . put    


However, some monads feature *control operations*, which are instructions that contain the monad `m` in the argument. Essentially, they can change the *control flow*. For example, the `MonadPlus` class contains an instruction

    mplus :: MonadPlus m => m a -> m a -> m a

that combines the control flows of two monadic arguments.

For more on the distinction between algebraic operation and control operation, see also a [discussion by Conor McBride][conor].

  [conor]: http://www.haskell.org/pipermail/haskell-cafe/2010-April/076185.html

### MonadReader

The main feature of the `MonadReader` class is an algebraic operation

    ask :: MonadReader m r => m r

but unfortunately, it also includes a control operation

    local :: MonadReader m r => (r -> r) -> m r -> m r

and it is not clear whether this can be lifted to the `ProgramT` transformer. We certainly expect the following law to hold

    local r (lift m) = lift (local f m)

Fortunately, this control operation is very benign, in that it is actually a monad morphism

    local r (return a) = return a
    local r (m >>= k)  = local r m >>= local r . k

Imposing that the lifted control operation should also be a morphism, we can define it for normal forms as follows

    local :: MonadReader m r => (r -> r)
          -> ProgramT instr m a -> ProgramT instr m a
    local r (JoinLift m) = JoinLift $ local r (m >>= return . f)
        where
        f (Return' a) = return a
        f (i :>>=' k) = singleton i >>= local r . k
        
Again, it is somewhat tedious to check that this definition fulfills the lifting and morphism laws. However, we have now succeeded in lifting a control operation!




