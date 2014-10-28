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

testBoard = ["**WWW","*-WW-","-----","-BB-*","BBB**"]
testRow   = (head testBoard)

testAsteriskString = "**WWW*-WW-------BB-*BBB**"


{-
TODO

crusher_o7m8 :: [String] -> Char -> Int -> Int -> IO()
crusher_o7m8 history side searchdepth n =
    printStrBoard( revertBoards(
		    stateSearch 
    		   (initializeBoard (head history)):[]
    		   (initializeBoards  (tail history)) 
		   side
		   searchdepth
		   n))


initializeBoards :: [String] -> [Board]
initializeBoards initstrings = 
     map initializeBoard initstrings

-- Takes in a single String that represents a hexagonal Crusher board
-- and converts it into a square Board with asterisks included
-- to represent empty spaces.

initializeBoard :: String -> Board
initializeBoard initstring =

-- Convert a list of our square Boards back to original String form.
-- Basically, the opposite of initializeBoards
revertBoards :: [Board] -> [String]
revertBoards boards =
     map revertBoard boards

revertBoard :: Board -> String
revertBoard board 


-- Generates best new Board state

stateSearch :: [Board] -> [Board] -> Char -> Int -> Int -> [Board]
stateSearch unexplored history side searchdepth n path
     | null unexplored || searchdepth == 0 = []
     | isWin (head unexplored)  =  
     |


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
     | null initBoard = acc
     | 


-- For the given row, check each char. 
-- If it matches side, check if it can move right
-- if it can, move it, add the new row to acc 
-- if not, move to next char
-- NOTE that acc and the return value are NOT Boards.

generateRightMovesFromRow :: Row -> Char -> [Row] -> [Row]
generateRightMovesFromRow row side acc






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

--TO TEST

-- Takes in a segment of a Row and checks whether the head character
-- can jump right 2 spaces.
-- It can jump right only if the spot 2 spaces away is NOT ally or out of bounds
canJumpRight :: Row -> Bool
canJumpRight row 
     | null row || null (tail row)   = False
     | null (tail (tail row)) 	     = False
     | otherwise  		     = (jumpSpot !! (head row)) 
       				    || (jumpSpot !! outOfBounds) 
       where jumpSpot = (head (tail (tail row))) 


-- TESTED AND WORKS

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

