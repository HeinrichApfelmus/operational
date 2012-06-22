{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    A reformulation of Koen Claessen's Parallel Parsing Processes
    http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.9217

    For a detailed explanation, see also
    http://apfelmus.nfshost.com/articles/operational-monad.html#monadic-parser-combinators
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types, TypeSynonymInstances #-}
module BreadthFirstParsing where

import Control.Monad
import Control.Monad.Operational

{------------------------------------------------------------------------------
    At their core, a parser monad consists of just three
    primitive instructions
    
        symbol -- fetch the next character
        mzero  -- indicate parse failure
        mplus  -- non-deterministic choice between two parsers
    
    and an interpreter function
        
        parse :: Parser a -> (String -> [a])
    
    that applies a parser to a string and returns
    all the possible parse results.
------------------------------------------------------------------------------}
data ParserInstruction a where
    Symbol :: ParserInstruction Char
    MZero  :: ParserInstruction a
    MPlus  :: Parser a -> Parser a -> ParserInstruction a

type Parser = Program ParserInstruction

symbol = singleton Symbol

instance MonadPlus Parser where
    mzero     = singleton $ MZero
    mplus x y = singleton $ MPlus x y

-- apply a parser to a string
-- breadth first fashion: each input character is touched only once
parse :: Parser a -> String -> [a]
parse p = go (expand p)
    where
    go :: [Parser a] -> String -> [a]
    go ps []     = [a | Return a <- map view ps]
    go ps (c:cs) = go [p | (Symbol :>>= is) <- map view ps, p <- expand (is c)] cs

-- keep track of parsers that are run in parallel
expand :: Parser a -> [Parser a]
expand p = case view p of
    MPlus p q :>>= k  -> expand (p >>= k) ++ expand (q >>= k)
    MZero     :>>= k  -> []
    _                 -> [p]


-- example
-- > parse parens "()(()())"
-- [()]     -- one parse
-- > parse parens "()((())"
-- []       -- no parse
parens :: Parser ()
parens = return () `mplus` (enclose parens >> parens)
    where
    enclose q = char '(' >> q >> char ')'

many :: Parser a -> Parser [a]
many p = mzero `mplus` liftM2 (:) p (many p) 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- symbol; if p c then return c else mzero

char c = satisfy (==c)
