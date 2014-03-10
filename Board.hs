-- Board.hs
-- Scott Bouloutian

-- A board is represented by a list of lists
-- Each index in the outer list is a section of the big board
-- Each index in the inner list is a section of the small board

module Board
where
 import Data.List
 data Board = Board {state :: [Char], lastMove :: (Int,Int)} deriving Show

 -- Returns an empty board
 emptyBoard :: Board
 emptyBoard = Board {state = take 81 (repeat '_'), lastMove = (-1,-1)}

 -- Returns a row (0 to 8) from the board
 displayRow :: Board -> Int -> IO()
 displayRow board row = putStrLn (intersperse ' ' [fst x | x <- (zipWithIndex (state board)), rowFromIndex (snd x) == row])

 -- Displays each row of the board
 displayBoard :: Board ->IO[()]
 displayBoard board = mapM (displayRow board) [0..8]

 -- Pairs each element in a list with its corresponding index
 zipWithIndex :: [a] -> [(a,Int)]
 zipWithIndex list = zip list [0..]

 -- Takes an index from 0 to 80 and makes it of the form (bigField,smallField)
 position :: Int -> (Int,Int)
 position i = (bigIndices i,smallIndices i)

 -- Takes an index from 0 to 80 and returns its corresponding big field index
 bigIndices :: Int -> Int
 bigIndices i = (quot (rowFromIndex i) 3) * 3 + (quot (colFromIndex i) 3)

 -- Takes an index from 0 to 80 and returns its corresponding small field index
 smallIndices :: Int -> Int
 smallIndices i = (mod (rowFromIndex i) 3) * 3 + mod (colFromIndex i) 3

 -- Takes an index from 0 to 80 and returns its row in a 9x9 grid
 rowFromIndex :: Int -> Int
 rowFromIndex i = quot i 9

 -- Takes an index from 0 to 80 and returns its column in a 9x9 grid
 colFromIndex :: Int -> Int
 colFromIndex i = mod i 9

 getIndex :: (Int,Int) -> Int
 getIndex (big,small) = row * 9 + col
                        where row = (quot big 3) * 3 + (quot small 3)
                              col = (mod big 3) * 3 + (mod small 3)

 getSector :: Int -> Board -> [Char]
 getSector n board = [fst x | x <- (zipWithIndex (state board)), bigIndices (snd x) == n]