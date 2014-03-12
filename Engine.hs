module Engine
where
 import Board

 -- Determines whether a move on the given board is valid
 validMove :: (Int,Int) -> Board -> Bool
 validMove (m1,m2) board = inBounds m1 && inBounds m2 && (snd (lastMove board) == -1 || m1 == snd (lastMove board))

 inBounds :: Int -> Bool
 inBounds n = n>=0 && n<9

 -- Performs the given move on the given board
 performMove :: (Int,Int) -> Char -> Board -> Board
 performMove move token board = Board {state = left ++ [token] ++ right, lastMove = move}
                          where left = fst split
                                right = tail (snd split)
                                split = splitAt (getIndex move) (state board)

 -- Checks to see if the board is won
 winner :: Char -> Board -> Bool
 winner token board = any ((==) True) [sectorWin (getSector x board) token board | x <- [0..8]]
 
 sectorWin :: [Char] -> Char -> Board -> Bool
 sectorWin sector token board = any ((==) True) ([rowWin x token sector | x <- [0,1,2]] 
 	                                          ++ [colWin x token sector | x <- [0,1,2]]
 	                                          ++ [diagWin x token sector | x <- [0,1]])

 rowWin :: Int -> Char -> [Char] -> Bool
 rowWin row token list = all ((==) token) (rowFromSector row list)

 colWin :: Int -> Char -> [Char] -> Bool
 colWin col token list = all ((==) token) (colFromSector col list)

 diagWin :: Int -> Char -> [Char] -> Bool
 diagWin diag token list = all ((==) token) (diagFromSector diag list)

 rowFromSector :: Int -> [a] -> [a]
 rowFromSector row sector = [fst x | x <- zipWithIndex(sector), quot (snd x) 3 == row]

 colFromSector :: Int -> [a] -> [a]
 colFromSector col sector = [fst x | x <- zipWithIndex(sector), mod (snd x) 3 == col]

 diagFromSector :: Int -> [a] -> [a]
 diagFromSector 0 sector = [fst x | x <- zipWithIndex(sector), y <- [0,4,8], snd x == y]
 diagFromSector 1 sector = [fst x | x <- zipWithIndex(sector), y <- [2,4,6], snd x == y]
