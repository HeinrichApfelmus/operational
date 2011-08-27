#!/bin/sh runghc
\begin{code}
{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    A CGI script that maintains session state
    http://www.informatik.uni-freiburg.de/~thiemann/WASH/draft.pdf

------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types #-}
module WebSessionState where

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans hiding (lift)

import Data.Char
import Data.Maybe

    -- external libraries needed
import Text.Html as H
import Network.CGI

{------------------------------------------------------------------------------
    This example shows a "magic" implementation of a web session that
    looks like it needs to be executed in a running process,
    while in fact it's just a CGI script.
    
    The key part is a monad, called "Web" for lack of imagination,
    which supports a single operation
    
        ask :: String -> Web String
    
    which sends a simple minded HTML-Form to the web user
    and returns his answer.
    
    How does this work? The trick is that all previous answers
    are logged in a hidden field of the input form.
    The CGI script will simply replays this log when called.
    In other words, the user state is stored in the input form.

------------------------------------------------------------------------------}
data WebI a where
    Ask :: String -> WebI String

type Web a = Program WebI a

ask = singleton . Ask

    -- interpreter
runWeb :: Web H.Html -> CGI CGIResult
runWeb m = do
            -- fetch log
        log' <- maybe [] (read . urlDecode) `liftM` getInput "log"
            -- maybe append form input
        f    <- maybe id (\answer -> (++ [answer])) `liftM` getInput "answer"
        let log = f log'
            -- run Web action and output result
        output . renderHtml =<< replay m log log
    where
    replay = eval . view
    
    eval :: ProgramView WebI H.Html -> [String] -> [String] -> CGI H.Html
    eval (Return html)         log _      = return html
    eval (Ask question :>>= k) log (l:ls) = -- replay answer from log
        replay (k l) log ls
    eval (Ask question :>>= k) log []     = -- present HTML page to user
        return $ htmlQuestion log question


    -- HTML page with a single form
htmlQuestion log question = htmlEnvelope $ p << question +++ x
    where
    x = form ! [method "post"] << (textfield "answer"
                +++ submit "Next" ""
                +++ hidden "log" (urlEncode $ show log))

htmlMessage s = htmlEnvelope $ p << s

htmlEnvelope html =
    header << thetitle << "Web Session State demo"
    +++ body << html


    -- example
example :: Web H.Html
example = do
    haskell <- ask "What's your favorite programming language?"
    if map toLower haskell /= "haskell"
        then message "Awww."
        else do
            ghc <- ask "What's your favorite compiler?"
            web <- ask "What's your favorite monad?"
            message $ "I like " ++ ghc ++ " too, but "
                      ++ web ++ " is debatable."
    where
    message = return . htmlMessage

main = runCGI . runWeb $ example

\end{code}
