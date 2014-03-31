{-
	Engine.hs
	Scott Bouloutian

	This module defines the game logic behind Ultimate Tic-Tac-Toe
-}

module Engine(validMove,performMove,winner,rowFromSector,colFromSector,diagFromSector)
where
 import Board
 import Data.List

 -- Determines whether a move on the given board is valid by checking the following criteria
 --     1. The big index is in the bounds
 --     2. The small index is in the bounds
 --     3. The space is empty
 --     4. The sector has not yet been won
 --     5. The big index corresponds to the small index of the last move unless the sector of the big index is already won
 validMove :: (Int,Int) -> Board -> Bool
 validMove (m1,m2) board = inBounds m1
                           && inBounds m2
                           && state board !! getIndex (m1,m2) == '_'
                           && (sectorUtility $ getSector m1 board) == '_'
                           && (n == -1 || m1 == n || (sectorUtility $ getSector n board) /= '_')
                           where n = snd $ lastMove board

 -- Checks if an index is within its proper bounds
 inBounds :: Int -> Bool
 inBounds n = n>=0 && n<9

 -- Performs the given move on the given board and returns the resulting board
 performMove :: (Int,Int) -> Char -> Board -> Board
 performMove move token board = Board {state = left ++ [token] ++ right, lastMove = move}
                                where left = fst split
                                      right = tail (snd split)
                                      split = splitAt (getIndex move) (state board)

 -- Checks the board to see if the game is over and returns the winner
 winner :: Board -> Char
 winner board = sectorUtility [sectorUtility (getSector x board) | x <- [0..8]]
 
 -- Looks at an array of characters (conceptualized as a 3x3 Tic-Tac-Toe board) and returns the winner
 sectorUtility :: [Char] -> Char
 sectorUtility sector | sectorWin sector 'X' = 'X'
                      | sectorWin sector 'O' = 'O'
                      | sectorIsFull sector = 'T'
                      | otherwise = '_'

 -- Returns true if there are nto any empty spaces in the corresponding sector
 sectorIsFull :: [Char] -> Bool
 sectorIsFull sector = not (any ((==) '_') sector)

 -- Returns true if someone has won the corresponding sector
 sectorWin :: Eq a => [a] -> a -> Bool
 sectorWin sector token= any ((==) True) ([allEqual token (rowFromSector x sector) | x <- [0,1,2]] 
 	                                   ++ [allEqual token (colFromSector x sector) | x <- [0,1,2]]
 	                                   ++ [allEqual token (diagFromSector x sector) | x <- [0,1]])

 -- Returns true is all the tokens in a given list are equal to the given token
 allEqual :: Eq a => a -> [a] -> Bool
 allEqual token list = all ((==) token) list

 -- Retuurns a specific row given a sector
 rowFromSector :: Int -> [a] -> [a]
 rowFromSector row sector = [fst x | x <- zipWithIndex(sector), quot (snd x) 3 == row]

 -- Returns a specific column given a sector
 colFromSector :: Int -> [a] -> [a]
 colFromSector col sector = [fst x | x <- zipWithIndex(sector), mod (snd x) 3 == col]

 -- Returns a specific diagonal given a sector
 diagFromSector :: Int -> [a] -> [a]
 diagFromSector 0 sector = [fst x | x <- zipWithIndex(sector), y <- [0,4,8], snd x == y]
 diagFromSector 1 sector = [fst x | x <- zipWithIndex(sector), y <- [2,4,6], snd x == y]
