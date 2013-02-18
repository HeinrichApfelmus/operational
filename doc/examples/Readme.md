Example Code for the *operational* package
==========================================

<dl>
<dt><a href="BreadthFirstParsing.hs">BreadthFirstParsing.hs</a>
    <dd>An breadth-first implementation of parser combinators.
    As this implementation does not back-track, we avoid a common space leak.
<dt><a href="LogicT.hs">LogicT.hs</a>
    <dd>Oleg Kiselyov's <code>LogicT</code> monad transformer.
<dt><a href="ListT.hs">ListT.hs</a>
    <dd>Correct implementation of the list monad transformer.
<dt><a href="PoorMansConcurrency.hs">PoorMansConcurrency.hs</a>
    <dd>Koen Claessen's poor man's concurrency monad, implements cooperative multitasking.
<dt><a href="State.hs">State.hs</a>
    <dd>Very simple example showing how to implement the state monad.
<dt><a href="TicTacToe.hs">TicTacToe.hs</a>
    <dd>The game of TicTacToe. Mix and mash humans and AI as you like; players are implemented in a special monad that looks like there is only one player playing.
<dt><a href="WebSessionState.lhs">WebSessionState.lhs</a>
    <dd>CGI Script that is written in a style seems to require exeution in a persistent process, but actually stores a log of the session in the client.
</dl>
