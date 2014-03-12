module Main
where
 import Engine
 import Board

 -- Start of the program
 main = do putStrLn "Welcome to Ultimate Tic-Tac-Toe!"
           playerOneTurn emptyBoard

 -- Prompts the user for a move until a valid one is entered
 promptMove :: Board -> IO (Int,Int)
 promptMove  board = do putStrLn "Enter your move:"
                        bigField <- readLn
                        smallField <- readLn
                        putStrLn ""
                        if validMove (bigField,smallField) board
                        then return (bigField,smallField)
                        else promptMove board

 playerOneTurn :: Board -> IO()
 playerOneTurn board = do move <- promptMove board
                          let newBoard = performMove move 'X' board
                          displayBoard newBoard
                          putStrLn ""
                          if (not (winner 'X' newBoard))
                          then playerTwoTurn newBoard
                          else putStrLn "Game Over"

 playerTwoTurn :: Board -> IO()
 playerTwoTurn board = do move <- promptMove board
                          let newBoard = performMove move 'O' board
                          displayBoard newBoard
                          putStrLn ""
                          if (not (winner 'O' newBoard))
                          then playerOneTurn newBoard
                          else putStrLn "Game Over"

 displayBoard :: Board -> IO()
 displayBoard board = putStrLn (getBoardString board)