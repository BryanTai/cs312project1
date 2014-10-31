-- CPSC312 Project 1
-- Bryan Tai and Linh Phan
-- o7m8 and a5i8


import Data.List
 
-- constants
outOfBounds 	= '*'
blackPawn 	= 'B'
whitePawn 	= 'W'
emptySpace	= '-'



--   * * W W W 
--   * - W W - 
--   - - - - -
--   - B B - *
--   B B B * *




-- This will make our code easier to read
type Row   = String
type Board = [Row]

testBoard3n = ["**WWW","*-WW-","-----","-BB-*","BBB**"]
testRow   = (head testBoard3n)



testString3n = "WWW-WW-------BB-BBB"
testAsteriskString3n = "**WWW*-WW-------BB-*BBB**"

testString4n = "WWWW-WWW---------------------BBB-BBBB"

{-
--TODO

crusher_o7m8 :: [String] -> Char -> Int -> Int -> IO()
crusher_o7m8 history side searchdepth n =
    printStrBoard( revertBoards(
		    stateSearch 
    		   (initializeBoard (head history)):[]
    		   (initializeBoards  (tail history)) 
		   side
		   searchdepth
		   n))





-- Generates best new Board state

stateSearch :: [Board] -> [Board] -> Char -> Int -> Int -> [Board]
stateSearch unexplored history side searchdepth n path
     | null unexplored || searchdepth == 0 = []
     | isWin (head unexplored)  =  
     |

---------------------------------------------------------------------
-- Takes in a Crusher Board and the current history 
-- and returns the all the next possible Crusher Boards.

generateNewMoves :: Board -> [Board] -> Char -> [Board]
generateNewMoves initBoard history side =
     concat THE 6 GENERATE MOVES
     	    (generateRightMoves (head history) history side 0 [])



-- Returns all the possible Boards that can be created by moving
-- each pawn of the given side to the RIGHT.
-- Looks at each row, keep track of what number row we're on. 
-- If it has at least one side pawn, generate states from it.

generateRightMoves :: Board -> [Board] -> Char -> Int -> [Board] -> [Board]
generateRightMoves initBoard history side level acc
     | null initBoard           = acc
     | 


-- For the given row, check each char. 
-- If it matches side, check if it can move right
-- if it can, move it, add the new row to acc 
-- if not, move to next char
-- NOTE that acc and the return value are NOT Boards.
-- n is the index in a single row

generateRightMovesFromRow :: Row -> Char -> Int -> [Row] -> [Row]
generateRightMovesFromRow row side index acc 
    | null row                         = acc 
    | row !! index == side             = (generateRightMovesFromRow 
                                                    row 
                                                    side 
                                                    (index + 1) 
                                                    ((moveRight row index) : acc))
    | otherwise                         = (generateRightMoves 
                                                    row
                                                    side
                                                    (index + 1)
                                                    acc)




-- For the given row, shift all elements in that row
-- to the right.
moveRight :: Row -> Int -> Row
moveRight row index
    | (canMoveRight row)                 = moveRight_helper row 
    | (canJumpRight row)                 = jumpRight_helper row 
    | otherwise                          = []

-- 
moveRight_helper :: Row -> Row
moveRight_helper row
    | (head (tail row)) != emptySpace                  = row
    | otherwise                                        = emptySpace:(head row):(tail (tail row))

-- Jumps an element from a row over another element 
jumpRight_helper :: Row -> Row
jumpRight_helper row
    | (head (tail row) != emptySpace                   = row
    | otherwise                                        = (emptySPace:(head (tail row)):(head row)) ++ (tail (tail row))


-- takes all the generated right moves from a single row from the accumulator and generates all the different possibilties of pawns movement for that single row

createNewBoardsFromRows :: [Row] -> Int -> Board -> [Board]
createNewBoardsFromRows rows level initboard 

generateLeftMoves

generateUpLeftMoves

generateDownRightMoves

--TODO: Cannot rely on generateRightMoves
generateUpRightMoves
generateDownLeftMoves



-}


-- VVVVV TO TEST VVVVV


-- VVVVV  TESTED AND WORKS VVVVV

{- ********** BOARD STATE FUNCTIONS ************** -}

-- Takes in a single String that represents a hexagonal Crusher board
-- with side length n and converts it into a square Board with
-- asterisks included to represent empty spaces.
initializeBoard :: Int -> String -> Board
initializeBoard n initstring =
     addOutOfBounds initstring n 1 []

-- Initializes a whole list of Strings into a list of Boards.
initializeBoards :: Int -> [String] -> [Board]
initializeBoards n initstrings = 
     map f initstrings
     where f = initializeBoard n

-- Convert a list of our square Boards back to original String form.
-- Basically, the opposite of initializeBoards
revertBoards :: [Board] -> [String]
revertBoards boards =
     map revertBoard boards

revertBoard :: Board -> String
revertBoard board =
     filter (/=outOfBounds) (concat board) 


-- Takes in a String representing a Board with side length n.
-- Splits it up and returns a Board with asterisks added to 
-- represent the outOfBounds areas.
-- Initial index is 1, increases until it is 2n-1
-- Don't worry about the math. I did all the math. It works.
addOutOfBounds :: String -> Int -> Int -> [Row] -> Board
addOutOfBounds initstring n index rows
     | null initstring    = (reverse rows)
     | index <= n      	  = (recurse (n+index-1)
       	     		    (newTopRow initstring n index))  
     | otherwise 	  = (recurse ((3*n)-index-1) 
       			    (newBotRow initstring n index))
      where recurse newIndex newRow = addOutOfBounds 
       	       		      	      (tailXOfString initstring newIndex) 
       	       		      	      n 
			      	      (index+1) 
       	       		      	      (newRow:rows)

-- Creates a new top row of the board with asterisks added
-- Note that (n - index) + (n + index - 1) = 2n-1

newTopRow :: String -> Int -> Int -> Row
newTopRow initstring n index =
     (makeOOB (n - index)) ++ 
     (headXOfString initstring (n + index - 1))

-- Creates a new bottom row of the board with asterisks added
-- Note that (3n - index - 1) + (index - n) = 2n-1

newBotRow :: String -> Int -> Int -> Row
newBotRow initstring n index =
     (headXOfString initstring ((3*n) - index - 1)) ++
     (makeOOB (index - n))

-- Gets the first x elements of a String
headXOfString :: String -> Int -> String
headXOfString init x =
     (fst (splitAt x init))

-- Gets the last elements after index x
tailXOfString :: String -> Int -> String
tailXOfString init x =
     (snd (splitAt x init))

-- make String of outOfBounds with length x 
makeOOB :: Int -> String
makeOOB x =
     replicate x outOfBounds

-- ********************************************

-- Splits a List at regular intervals
-- Borrowed from Haskell wiki
-- DO NOT HAND IT ZERO
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

-- Takes in a segment of a Row and checks whether the head character
-- can jump right 2 spaces.
-- head char should be either whitePawn or blackPawn
-- It can jump right only if the spot 2 spaces away is NOT ally or out of bounds
canJumpRight :: Row -> Bool
canJumpRight row 
     | null row || null (tail row)     = False
     | null (tail (tail row)) 	       = False
     | (head row) /= (head (tail row)) = False
     | otherwise  		       = (jumpSpot /= (head row)) 
       				      && (jumpSpot /=  outOfBounds) 
       where jumpSpot = (head (tail (tail row))) 

-- Takes in a segment of a Row and checks whether the head character
-- can move right one spot
-- head char should be either whitePawn or blackPawn
-- It can move right only if the adjecent spot is empty  
canMoveRight :: Row -> Bool
canMoveRight row
     | null row || null (tail row)   = False
     | otherwise  	      	     = (head (tail row)) == emptySpace



-- PRINT NICE -------------------
-- Code from Connect Discussion
-- Authored and Shared by Kurt's former student. Bless his heart.
-- Recycled from RushHour

-- Get a list of lists of strings and output them nicely.      

printStrMatrix :: [Board] -> IO ()        
printStrMatrix [] = printStrBoard []
printStrMatrix (x:xs) = do
        printStrBoard x
        printStrMatrix xs

-- Print nicely a list of strings. 
-- Eg: printStrBoard ["aabb", "ccdd", "eeff"] prints in the console:
-- aabb
-- ccdd
-- eeff

printStrBoard :: Board -> IO ()
printStrBoard [] = putStrLn ""
printStrBoard (x:xs) = do 
        putStrLn x
        printStrBoard xs         

