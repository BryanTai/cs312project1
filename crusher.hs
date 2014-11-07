-- CPSC312 Project 1
-- Bryan Tai and Linh Phan
-- o7m8 and a5i8


import Data.List
 
-- constants
outOfBounds 	= '*'
blackPawn 	    = 'B'
whitePawn 	    = 'W'
emptySpace	    = '-'



--   * * W W W 
--   * - W W - 
--   - - - - -
--   - B B - *
--   B B B * *




-- This will make our code easier to read
type Row   = String
type Board = [Row]


--Test Values

testString3n = "WWW-WW-------BB-BBB"
testBoard3n = ["**WWW","*-WW-","-----","-BB-*","BBB**"]
testRow   = (head testBoard3n)

testBoard3nWhite = ["**WWW","*-WW-","-----","----*","BB-**"]

testAsteriskString3n = "**WWW*-WW-------BB-*BBB**"

testString3nFirst = "-WW-WW---W---BB-BBB"
testBoard3nFirst  = ["**-WW","*-WW-","--W--","-BB-*","BBB**"]

testString4n = "WWWW-WWW---------------------BBB-BBBB"



--TODO

--Returns an updated Board HIstory with our new move at the head
crusher_o7m8 :: [String] -> Char -> Int -> Int -> [String]
crusher_o7m8 history side searchdepth n =
    printStrBoard( revertBoards(
		    stateSearch 
    		   (initializeBoard (head history)):[]
    		   (initializeBoards  (tail history)) 
		   side
		   searchdepth
		   n))
--also check if inital Board is gameOver



---------------------------------------------------------------------
-- Takes in a Crusher Board and the current history 
-- and returns the all the next possible Crusher Boards.
-- If NO moves returned, then the given Crusher board is in a position
-- where no moves are possible for the given side (thus, they lose)
generateNewMoves :: Board -> [Board] -> Char -> [Board]
generateNewMoves initBoard history side =
     concat THE 6 GENERATE MOVES
     	    (generateRightMoves (head history) history side 0 [])


-- NOT FINISHED
-- Returns all the possible Boards that can be created by moving
-- each pawn of the given side in initBoard to the RIGHT.
-- Looks at each row, keep track of what number row we're on. 
-- If it has at least one side pawn, generate states from it.

generateRightMoves :: [Board] -> Char -> Int -> [Board] -> [Board]
generateRightMoves history side level acc
     | level == (length initBoard)             = acc
     | elem side (initBoard !! level)          = (generateRightMoves 
                                               history
                                               side
                                               (level + 1)
                                               (createBoardsFromRightMoves
                                               initboard
                                               level
                                               (generateRightMovesFromRow (initboard !! level) side 0 []):acc
                                               ))

     | otherwise                            = (generateRightMoves
                                               history
                                               side
                                               (level + 1)
                                               acc)


     where initboard = (head history)

createBoardsFromRightMoves :: Board -> Int -> [Row] -> [Board]
createBoardsFromRightMoves initBoard level rows 
    | null rows              = []
    | otherwise              = (createBoardFromRightMove (initboard levels (tail rows)))

-- takes a single row and creates a new board with the row it is being replaced with
createBoardFromRightMove :: Board -> Row -> [Row] -> Int -> Board
createBoardFromRightMove initboard row acc level 
  | level == 0        = (level - 1)
  | otherwise         =



generateLeftMoves :: Board -> Char -> Int -> [Board] -> [Board]
generateLeftMoves board side index acc
    map reverseBoard 
        (generateRightMoves (reverseBoard board) side (index + 1) acc))


reverseBoard :: Board -> [Row]
reverseBoard board = map reverse board

generateUpLeftMoves 

generateDownRightMoves

--TODO: Cannot rely on generateRightMoves
--use swapBoardAsterisks
generateUpRightMoves
generateDownLeftMoves

**-WW
*-WW-
--W--
-BB-*
BBB**




-- Generates the best new Board state given game history information
-- Uses the MINMAX algorithm up to a certain searchdepth
-- First generates all the possible moves up til searchdepth
-- (not considering those that are repeats)
-- Called on EACH possible board.
-- Initial call, findMax is True. Alternates each level down

stateSearch :: [Board] -> Char -> Int -> Int -> Bool -> Board
stateSearch history side searchdepth n findMax
     | (null (head history)) || (searchdepth == 1)   = []
     | 

-- find the best board of the LEAF moves
	getMinMaxBoard (generateNewMoves (head unexplored) history side)
		       side --or otherSide?
		       n
		       findMax
		       (getMinMaxScore genBoards side n (if findMax 
		       		       		      	then maximum 
							else minimum))
		
-- find the best board of NEXT moves
	getMinMaxBoard (stateSearch (head generateNewMoves)
		       		    (otherSide side)
				    (searchDepth-1)
       				    n
				    (not findMax))

--recursive call
    (stateSearch (tail unexplored) 
     		 ((head unexplored):history)
     		 (otherSide side) 
		 (searchDepth-1)
		 n
		 (not findMax)





-- Takes in a list of generated boards and returns the BOARD with either
-- the highest or lowest calculated score based on minmax heuristic.
-- minmax is either minimum or maximum
getMinMaxBoard :: [Board] -> Char -> Int -> Bool -> Int -> Board 
getMinMaxBoard genBoards side n findMax score =
     | 

-}


-- VVVVV TO TEST VVVVV


-- Takes in a list of generated boards and returns either
-- the highest or lowest calculated SCORE based on minmax heuristic.
-- minmax is either minimum or maximum
getMinMaxValue :: [Board] -> Char -> Int -> ([Int] -> Int) -> Int 
getMinMaxValue genBoards side n minmax =
    minmax (map k genBoards)    
    where k = (boardEvaluator side n)

-- Checks whether a board is already over due to one side having
-- less than n pawns
gameOver :: Board -> Int -> Bool
gameOver initBoard n = 
     (isCrushedWin initBoard whitePawn n) ||
     (isCrushedWin initBoard blackPawn n)

-- Static Board Evaluator
-- Takes in a Board and relevant information. Returns a score based
-- on our heuristic.
-- Cannot look ahead moves (ie. generate new states)
-- Will not be passed a board that has already been seen by history
-- Whoever calls this function should take care of that maybe
-- <post heuristic here>
boardEvaluator :: Char -> Int -> Board -> Int
boardEvaluator side n initBoard 
     | (isCrushedWin initBoard side n)		   =  1000
     | (isCrushedWin initBoard (otherSide side) n) = -1000
     | otherwise     	= calculateScore initBoard side 


-- (# of given side's pawns times 10) minus (enemy pawns times 10)
--
-- TODO check for adjacent pawns
-- TODO check if pawns in middle / edge
calculateScore :: Board -> Char -> Int
calculateScore initBoard side =
    ((getPawnsForSide initBoard side) * 10) -
    ((getPawnsForSide initBoard (otherSide side)) * 10)

-}

-- VVVVV TO TEST VVVVV


-- Takes in a Board with length n and reverses the orientation of the asterisks
-- for UR and DL movements.
swapBoardAsterisks :: Board -> Int -> Board
swapBoardAsterisks initBoard n =
     reverse (addOutOfBounds 
     	     (reverse (revertBoard (map (reverse) initBoard))) 
	     n 1 [])

-- Takes in a swapped Board and sets it back to a regular Board
unswapBoardAsterisks :: Board -> Int -> Board
unswapBoardAsterisks swappedBoard n =
     addOutOfBounds 
     	     (revertBoard swappedBoard) 
	     n 1 []



-- VVVVV  TESTED AND WORKS VVVVV


{- ********** STATIC BOARD EVALUATOR FUNCTIONS ************** -}

-- Give a Board, returns the number of Pawns of the given side.
getPawnsForSide :: Board -> Char -> Int
getPawnsForSide initBoard side =
     (length (filter (side ==)(revertBoard initBoard)))

-- If the # of enemy pawns is less than n, the given side wins the Board.
isCrushedWin :: Board -> Char -> Int -> Bool
isCrushedWin initBoard side n 
     = n > (getPawnsForSide initBoard (otherSide side))

-- Given a pawn colour, simply returns the character of the other team. 
-- Input is either 'W' or 'B'
otherSide :: Char -> Char
otherSide side 
     | side == whitePawn	= blackPawn
     | otherwise 		= whitePawn

{- ********** MOVE GENERATION FUNCTIONS ************** -}

-- For the given row, check each char. 
-- If it matches side, check if it can move right
-- if it can, move it, add the new row to acc 
-- if not, move to next char
-- NOTE that acc and the return value are NOT Boards.
-- n is the index in a single row
-- intial index starts at 0

generateRightMovesFromRow :: Row -> Char -> Int -> [Row] -> [Row]
generateRightMovesFromRow row side index acc 
    | index  == (length row)              = filter (/= "") acc
    | (row !! index) == side               = (generateRightMovesFromRow 
                                                    row 
                                                    side 
                                                    (index + 1) 
                                                    ((moveRight row index) : acc))
    | otherwise                         = (generateRightMovesFromRow 
                                                    row
                                                    side
                                                    (index + 1)
                                                    acc)



-- takes in a partial row and moves the first element to the right by 1
-- requires that the given row can be moved right
moveRight_helper :: Row -> Row
moveRight_helper row = emptySpace:(head row):(tail (tail row))

---- Jumps an element from a row over another element 
jumpRight_helper :: Row -> Row
jumpRight_helper row = (emptySpace:(head (tail row)):(head row):[]) ++ (tail (tail (tail row)))

-- For the given row, shifts or jumps the element at the index to the right if it is a legal move.
-- Returns "" if the move is not legal.
moveRight :: Row -> Int -> Row
moveRight row index
    | (canMoveRight realRow)                    = (headXOfString row index) ++ (moveRight_helper realRow) 
    | (canJumpRight realRow)                   = (headXOfString row index) ++ (jumpRight_helper realRow)
    | otherwise                                 = []
        where realRow = (tailXOfString row index)


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

{-
-- Takes in a String representing a Board with side length n.
-- Splits it up and returns a Board with asterisks added to 
-- represent the outOfBounds areas.
-- Initial index is 1, increases until it is 2n-1
-- Don't worry about the math. I did all the math. It works.
addOutOfBounds :: String -> Int -> Int -> [Row] -> Board
addOutOfBounds initstring n index rows
     | null initstring    = (reverse rows)
     | index <= n      	  = (recurse 
       	     		    (n+index-1)
       	     		    (newTopRow initstring n index))  
     | otherwise 	  = (recurse 
       			    ((3*n)-index-1) 
       			    (newBotRow initstring n index))
      where recurse newIndex newRow = addOutOfBounds 
       	       		      	      (tailXOfString initstring newIndex) 
       	       		      	      n 
			      	      (index+1) 
       	       		      	      (newRow:rows)
-}

-- Takes in a String representing a Board with side length n.
-- Splits it up and returns a Board with asterisks added to 
-- represent the outOfBounds areas.
-- Initial index is 1, increases until it is 2n-1
-- Don't worry about the math. I did all the math. It works.
addOutOfBounds :: String -> Int -> Int -> [Row] -> Board
addOutOfBounds initstring n index rows
     | null initstring    = (reverse rows)
     | index <= n      	  = addOutOfBounds
       	     		    (tailXOfString initstring (n+index-1)) 
       	       		    n 
			    (index+1) 
       	       		    ((newTopRow initstring n index):rows)
     | otherwise 	  = addOutOfBounds
       	     		    (tailXOfString initstring ((3*n)-index-1)) 
       	       		    n 
			    (index+1) 
       	       		    ((newBotRow initstring n index):rows)



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
     | null row || null (tail row)    = False
     | otherwise  	      	          = (head (tail row)) == emptySpace



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

