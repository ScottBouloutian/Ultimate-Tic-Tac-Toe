{-
    GameAI.hs
    Scott Bouloutian

    This module serves as the starting point for implementing the AI minimax algorithm in Haskell.
    Due to my lack of expertise in Haskell, I am unclear as to whether or not this is the most efficient
    implementation of minimax in Haskell. It doesn't seem to have to capability to look very far ahead in
    the game tree, although this could be due to the branching factor of Ultimate Tic-Tac-Toe. The heuristic
    function is implemented but incomplete.

-}

module GameAI
where
 import Board
 import Engine

 possibleMoves :: Board -> [(Int,Int)]
 possibleMoves board = [(m1,m2) | m1 <- [0..8], m2 <- [0..8], validMove (m1,m2) board == True]

 utility :: Char -> Int
 utility token | token == 'X' = 1
               | token == 'O' = -1
               | token == 'T' = 0
               | otherwise = 0

 children :: Board -> Char -> [Board]
 children node token = [performMove move token node | move <- possibleMoves node]

 alphabeta :: Board -> Int-> Int -> Int-> Bool -> Int
 alphabeta node depth alpha beta player = if depth == 0 || winner node /= '_'
                                          then heuristic node player
                                          else if player
                                          	   then alphaLoop (children node 'X') alpha beta depth
                                          	   else betaLoop (children node 'O') alpha beta depth

 alphaLoop :: [Board] -> Int -> Int -> Int -> Int
 alphaLoop children alpha beta depth = if null children
 	                                   then alpha
 	                                   else if beta <= newAlpha
 	                             	        then alphaLoop [] newAlpha beta depth
 	                             	        else alphaLoop (tail children) newAlpha beta depth
                                	        where newAlpha = maximum [alpha, alphabeta (head children) (depth - 1) alpha beta False]

 betaLoop :: [Board] -> Int -> Int -> Int -> Int
 betaLoop children alpha beta depth = if null children
 	                                  then beta
 	                                  else if newBeta <= alpha
 	                            	       then betaLoop [] alpha newBeta depth
                                 	       else betaLoop (tail children) alpha newBeta depth
                                	       where newBeta = minimum [beta, alphabeta (head children) (depth - 1) alpha beta True]

 numTokensInRow :: Eq a => Int -> a -> [a] -> Int
 numTokensInRow row token list = length (filter ((==) token) (rowFromSector row list))

 numTokensInCol :: Eq a => Int -> a -> [a] -> Int
 numTokensInCol col token list = length (filter ((==) token) (colFromSector col list))

 numTokensInDiag :: Eq a => Int -> a -> [a] -> Int
 numTokensInDiag diag token list = length (filter ((==) token) (diagFromSector diag list))

 heuristic :: Board -> Bool -> Int
 heuristic board player = if player
 	                      then (-1) * sum [sectorHeuristic (getSector n board) 'X' | n <- [0..8]]
 	                      else sum [sectorHeuristic (getSector n board) 'O' | n <- [0..8]]

 sectorHeuristic :: [Char] -> Char -> Int
 sectorHeuristic sector token = sum ([fun (numTokensInRow x token sector) | x <- [0,1,2]]
 	                              ++ [fun (numTokensInCol x token sector) | x <- [0,1,2]]
 	                              ++ [fun (numTokensInDiag x token sector) | x <- [0,1]])

 fun x | x == 0 = 0
       | x == 1 = 1
       | x == 2 = 10
       | x == 3 = 100
       | otherwise = 0

 aiMove :: Board -> [(Int,Int)]
 aiMove board = [fst possible | possible <- possibles, (snd possible) == (maximum $ map snd possibles)]
 	            where possibles = zip (possibleMoves board) ([alphabeta (performMove move 'O' board) 2 (-99999) 99999 False | move <- possibleMoves board])