{-
    Game.hs
    Scott Bouloutian

    This module serves as the starting point of the Ultimate Tic-Tac-Toe game.
    Calling main launches the game.

    Ultimate Tic-Tac-Toe is a twist on the classic game of Tic-Tac-Toe.
    The playing board starts out as a three by three grid of empty tic-tac-toe boards.
    The game adheres to the following rules:

        - In each turn, the player marks one of the small squares.
        - When a player gets three in a row on a small board, the player wins that board.
        - To win the game, a player needs to win three small boards in a row.
        - A player may only play on the board in the big square that corresponds to the last
          small square the opponent played on.
        - If the board the player must play on is already won, the player may choose to play on any incomplete board.

-}

module Main(main)
where
 import Engine
 import Board
 import System.Environment (getArgs)
 import Text.Read (readMaybe)

 -- Start of the program
 main = do putStrLn "Welcome to Ultimate Tic-Tac-Toe!"
           playerOneTurn emptyBoard

 -- Prompts the user for a move until a valid one is entered
 promptMove :: Board -> IO (Int,Int)
 promptMove  board = do putStrLn "Enter your move:"
                        bigField <- getLine
                        smallField <- getLine
                        putStrLn ""
                        case parseArgs [bigField,smallField] of
                        	Left message->promptMove board
                        	Right move-> if validMove (0,0) board
                                         then return (0,0)
                                         else promptMove board

 -- Parses a list of arguments into two integers
 parseArgs :: [String] -> Either String (Int,Int)
 parseArgs input
   | length input /= 2 = Left "Wrong number of args"
   | otherwise = case (readMaybe arg1, readMaybe arg2) of
                 (Nothing,_) -> Left "arg1 invalid"
                 (_,Nothing) -> Left "arg2 invalid"
                 (Just a, Just b) -> Right (a,b)
   where
     arg1 = head input
     arg2 = input !! 1

 -- Performs player one's turn by prompting for a move and executing it
 playerOneTurn :: Board -> IO()
 playerOneTurn board = do move <- promptMove board
                          let newBoard = performMove move 'X' board
                          displayBoard newBoard
                          putStrLn ""
                          if (winner newBoard == '_')
                          then playerTwoTurn newBoard
                          else putStrLn "Game Over"

 -- Performs player two's turn by prompting for a move and executing it
 playerTwoTurn :: Board -> IO()
 playerTwoTurn board = do move <- promptMove board
                          let newBoard = performMove move 'O' board
                          displayBoard newBoard
                          putStrLn ""
                          if (winner newBoard == '_')
                          then playerOneTurn newBoard
                          else putStrLn "Game Over"

 -- Aesthetically outputs a representation of the board to the console
 displayBoard :: Board -> IO()
 displayBoard board = putStrLn (getBoardString board)