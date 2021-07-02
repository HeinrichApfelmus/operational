{------------------------------------------------------------------------------
    Control.Monad.Operational
    
    Example:
    An implementation of the game TicTacToe.
    
    Each player (human, AI, ...) is implemented in a separate monad
    which are then intermingled to run the game. This resembles the
    PoorMansConcurrency.hs example.
    
    
    Many thanks to Yves Par`es and Bertram Felgenhauer
    http://www.haskell.org/pipermail/haskell-cafe/2010-April/076216.html

------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, Rank2Types #-}

import Control.Monad
import Control.Monad.Operational
import Control.Monad.State

import Data.Either
import Data.List (transpose, intersperse)

    -- external libraries needed
import System.Random

{------------------------------------------------------------------------------
    The Player monad for implementing players (human, AI, ...)
    provides two operations
    
        readBoard   -- read the current board position
        playMove    -- play a move

    to query the current board position and perform a move, respectively.
    
    Moreover, it's actually a monad transformer intended to be used over IO.
    This way, the players can perform IO computations.
------------------------------------------------------------------------------}
data PlayerI a where
    ReadBoard :: PlayerI Board
    PlayMove  :: Int -> PlayerI Bool
    
type Player m a = ProgramT PlayerI m a

readBoard = singleton ReadBoard
playMove  = singleton . PlayMove

    -- interpreter
runGame :: Player IO () -> Player IO () -> IO ()
runGame player1 player2 = eval' initialGameState player1 player2
    where
    eval' game p1 p2 = viewT p1 >>= \p1view -> eval game p1view p2
    
    eval :: GameState
         -> ProgramViewT PlayerI IO () -> Player IO ()
         -> IO ()
    eval game (Return _)            _  = return ()
    eval game (ReadBoard   :>>= p1) p2 = eval' game (p1 (board game)) p2
    eval game (PlayMove mv :>>= p1) p2 =
        case makeMove mv game of
            Nothing         -> eval' game (p1 False) p2
            Just game'
                | won game' -> let p = activePlayer game in
                               putStrLn $ "Player " ++ show p ++ " has won!"
                | draw game'-> putStrLn $ "It's a draw."
                | otherwise -> eval' game' p2 (p1 True)
    
    -- example: human vs AI
main = do
    g <- getStdGen
    runGame playerHuman (playerAI g)

{------------------------------------------------------------------------------
    TicTacToe Board type and logic
    
    The board looks like this:
    
    +---+---+---+   some squares already played on
    | 1 | 2 | 3 |   the empty squares are numbered
    +---+---+---+
    | 4 | 5 |OOO|
    +---+---+---+
    | 7 |XXX| 9 |
    +---+---+---+
------------------------------------------------------------------------------}
data Symbol = X | O deriving (Eq,Show)
type Square = Either Int Symbol
type Board = [[Square]]
data GameState = Game { board :: Board, activePlayer :: Symbol }

initialGameState :: GameState
initialGameState = Game (map (map Left) [[1,2,3],[4,5,6],[7,8,9]]) X

    -- list the possible moves to play
possibleMoves :: Board -> [Int]
possibleMoves board = [k | Left k <- concat board]

    -- play a stone at a square
makeMove :: Int -> GameState -> Maybe GameState
makeMove k (Game board player)
    | not (k `elem` possibleMoves board) = Nothing   -- illegal move
    | otherwise = Just $ Game (map (map replace) board) (switch player)
    where
    replace (Left k') | k' == k = Right player
    replace x                   = x

    switch X = O
    switch O = X

    -- has somebody won the game?
won :: GameState -> Bool
won (Game board _) = any full $ diagonals board ++ rows board ++ cols board
    where
    full [a,b,c] = a == b && b == c
    diagonals [[a1,_,b1],
               [_ ,c,_ ],
               [b2,_,a2]] = [[a1,c,a2],[b1,c,b2]]
    rows = id
    cols = transpose

    -- is the game a draw?
draw :: GameState -> Bool
draw (Game board _) = null (possibleMoves board)

    -- print the board
showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

showBoard :: Board -> String
showBoard board =
      unlines . surround "+---+---+---+"
    . map (concat . surround "|". map showSquare)
    $ board
    where
    surround x xs = [x] ++ intersperse x xs ++ [x]

printBoard = putStr . showBoard

{------------------------------------------------------------------------------
    Player examples
------------------------------------------------------------------------------}
    -- a human player on the command line
playerHuman :: Player IO ()
playerHuman = forever $ readBoard >>= liftIO . printBoard >> doMove
    where
    -- ask the player where to move
    doMove :: Player IO ()
    doMove = do
        liftIO . putStrLn $ "At which number would you like to play?"
        n <- liftIO getLine
        b <- playMove (read n)
        unless b $ do
            liftIO . putStrLn $ "Position " ++ show n ++ " is already full."
            doMove

    -- a random AI,
    -- also demonstrates how to use a custom StateT on top
    --   of the Player monad
playerAI :: Monad m => StdGen -> Player m ()
playerAI = evalStateT ai
    where
    ai :: Monad m => StateT StdGen (ProgramT PlayerI m) ()
    ai = forever $ do
        board <- lift $ readBoard
        n     <- uniform (possibleMoves board) -- select a random move
        lift $ playMove n
        where
        -- select one element at random
        uniform :: Monad m => [a] -> StateT StdGen m a
        uniform xs = do
            gen <- get
            let (n,gen') = randomR (1,length xs) gen
            put gen'
            return (xs !! (n-1))

