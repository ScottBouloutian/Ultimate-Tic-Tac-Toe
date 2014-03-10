module Engine
where
 import Board

 -- Determines whether a move on the given board is valid
 validMove :: (Int,Int) -> Board -> Bool
 validMove (m1,m2) board = inBounds m1 && inBounds m2 && (snd (lastMove board) == -1 || m1 == snd (lastMove board))

 inBounds :: Int -> Bool
 inBounds n = n>=0 && n<9

 -- Performs the given move on the given board
 performMove :: (Int,Int) -> Board -> Board
 performMove move board = Board {state = left ++ ['X'] ++ right, lastMove = move}
                          where left = fst split
                                right = tail (snd split)
                                split = splitAt (getIndex move) (state board)

 -- Checks to see if the board is won
 winner :: Board -> Int
 winner board = 0

 rowWin :: Int -> Char -> [Char] -> Bool
 rowWin row element list = all ((==) element) (rowFromSector row list)

 rowFromSector :: Int -> [Char] -> [Char]
 rowFromSector row sector = [fst x | x <- zipWithIndex(sector), quot (snd x) 3 == row]

