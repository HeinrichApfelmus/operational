*Operational* is a tiny library for implementing monads by specifying the primitive instructions and their operational semantics. The monad laws will hold automatically. It can also be used to define monad transformers, and the lifting laws are, again, automatic.

It is based on the article [The Operational Monad Tutorial][tutorial], published in [Issue 15 of The Monad.Reader][reader].

For more, see the **[project website]**.

  [project website]: http://wiki.haskell.org/Operational

----

Sources and inspiration for this library include [Chuan-kai Lin's unimo paper][unimo], [John Hughes 95][hughes], and [Ryan Ingram's `MonadPrompt` package][prompt].

  [reader]: http://themonadreader.wordpress.com/2010/01/26/issue-15/
  [tutorial]: http://apfelmus.nfshost.com/articles/operational-monad.html
  [unimo]: http://web.cecs.pdx.edu/~cklin/papers/unimo-143.pdf "Chuan-kai Lin. Programming Monads Operationally with Unimo."
  [hughes]: http://citeseer.ist.psu.edu/hughes95design.html "John Hughes. The Design of a Pretty-printing Library."
  [prompt]: http://hackage.haskell.org/package/MonadPrompt "Ryan Ingram's Monad Prompt Package."
