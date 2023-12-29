[![Hackage](https://img.shields.io/hackage/v/operational.svg)](https://hackage.haskell.org/package/operational)

**Operational** is a tiny library for **implementing monads** by specifying the primitive instructions and their operational semantics. The monad laws will hold automatically. It can also be used to define monad transformers where the lifting laws hold automatically.

Using operational semantics simplifies the implementation of monads with tricky control flow, such as:

* web applications in sequential style
* games with a uniform interface for human and AI players, and automatic replay
* fast parser monads
* monadic DSLs of any kind
* ...

Any monad and monad transformer can be implemented in this fashion.

The library is based on the article [The Operational Monad Tutorial][tutorial], published in [Issue 15 of The Monad.Reader][reader].

## Example

For example, imagine that you want to write a web application where the user is guided through a sequence of tasks ("wizard"). To structure your application, you can use a custom monad that supports an instruction `askUserInput :: CustomMonad UserInput`. This command sends a web form to the user and returns a result when he submits the form. However, you don't want your server to block while waiting for the user, so you have to suspend the computation and resume it at some later point. Sounds tricky to implement? This library makes it easy:

The idea is to identify a set of primitive instructions and to specify their operational semantics. Then, the library makes sure that the monad laws hold automatically. In the web application example, the primitive instruction would be `AskUserInput`.

The above example is implemented in [WebSessionState.lhs](doc/examples/WebSessionState.lhs).

## Documentation

[More documentation and examples are included in the `doc/` folder](doc/).


## Related

Sources and inspiration for this library include

* [Chuan-kai Lin's unimo paper][unimo]
* J. Hughes, [The Design of a Pretty-printing Library][hughes], (1995)
* [Ryan Ingram's `MonadPrompt` package][prompt].

Related packages included

* [MonadPrompt](http://hackage.haskell.org/package/MonadPrompt)
* [free](http://hackage.haskell.org/package/free)
* [free-operational](http://hackage.haskell.org/package/free-operational)

  [reader]: http://themonadreader.wordpress.com/2010/01/26/issue-15/
  [tutorial]: http://apfelmus.nfshost.com/articles/operational-monad.html
  [unimo]: http://web.cecs.pdx.edu/~cklin/papers/unimo-143.pdf "Chuan-kai Lin. Programming Monads Operationally with Unimo."
  [hughes]: http://citeseer.ist.psu.edu/hughes95design.html "John Hughes. The Design of a Pretty-printing Library."
  [prompt]: http://hackage.haskell.org/package/MonadPrompt "Ryan Ingram's Monad Prompt Package."
