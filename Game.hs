module Main
where
 import Engine
 import Board

 -- Start of the program
 main = do putStrLn "Welcome to Ultimate Tic-Tac-Toe"
           playerOneTurn emptyBoard

 -- Prompts the user for a move until a valid one is entered
 promptMove :: Board -> IO (Int,Int)
 promptMove  board = do putStrLn "Enter your move: "
                        bigField <- readLn
                        smallField <- readLn
                        if validMove (bigField,smallField) board
                        then return (bigField,smallField)
                        else promptMove board

 playerOneTurn :: Board -> IO[()]
 playerOneTurn board = do move <- promptMove board
                          let newBoard = performMove move board
                          displayBoard newBoard
                          playerOneTurn newBoard