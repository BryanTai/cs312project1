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

testListOfBoard3n = [testBoard3n, testBoard3nFirst, testBoard3nWhite]

testBoard3nEmpty = ["**--","*--B-","--W--","----*","---**"]



----TODO

----Returns an updated Board HIstory with our new move at the head
--crusher_o7m8 :: [String] -> Char -> Int -> Int -> [String]
--crusher_o7m8 history side searchdepth n =
--    printStrBoard( revertBoards(
--		    stateSearch 
--    		   (initializeBoard (head history)):[]
--    		   (initializeBoards  (tail history)) 
--		   side
--		   searchdepth
--		   n))
----also check if inital Board is gameOver_o7m8



---------------------------------------------------------------------



-- Generates the best new Board state given game history information
-- Uses the MINMAX algorithm up to a certain searchdepth
-- First generates all the possible moves up til searchdepth
-- (not considering those that are repeats)
-- Called on EACH possible board.
-- Initial call, findMax is True. Alternates each level down

--stateSearch :: [Board] -> Char -> Int -> Int -> Bool -> Board -> Board
--stateSearch history side searchdepth n findMax initBoard
--     | (searchdepth == 0)   = initBoard
--     | otherwise       	    = getMinMaxBoard (map f (generateNewMoves initBoard history side)) side n findMax)  	
--   where f = (stateSearch (initBoard:history)
--     		    	  (otherSide side) 
--		    	  (searchDepth-1)
--		    	  n
--		    	  (not findMax))

---}


-- VVVVV TO TEST VVVVV

-- VVVVV  TESTED AND WORKS VVVVV



{- ********** STATIC BOARD EVALUATOR FUNCTIONS ************** -}

-- Takes in a list of generated boards and returns the BOARD with either
-- the highest or lowest calculated score based on minmax heuristic.
-- minmax is either minimum or maximum
getMinMaxBoard :: [Board] -> Char -> Int -> Bool -> Board 
getMinMaxBoard genBoards side n findMax 
     | ((boardEvaluator side n (head genBoards)) == 
        (getMinMaxValue genBoards side n findMax))        = (head genBoards) 
     | otherwise   = (getMinMaxBoard (tail genBoards) side n findMax)

-- Takes in a list of generated boards and returns either
-- the highest or lowest calculated SCORE based on minmax heuristic.
-- minmax is either minimum or maximum
getMinMaxValue :: [Board] -> Char -> Int -> Bool -> Int 
getMinMaxValue genBoards side n findMax =
    getMinMax findMax (map k genBoards)    
    where k = (boardEvaluator side n)

--Evaluates to maximum or minimum depending on given boolean
getMinMax :: Bool -> [Int] -> Int
getMinMax findMax 
     | findMax   = maximum 
     | otherwise = minimum

-- Checks whether a board is already over due to one side having
-- less than n pawns
gameOver_o7m8 :: Board -> Int -> Bool
gameOver_o7m8 initBoard n = 
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


-- Takes in a Crusher Board and the current history 
-- and returns the all the next possible Crusher Boards.
-- If NO moves returned, then the given Crusher board is in a position
-- where no moves are possible for the given side (thus, they lose)

generateNewMoves :: [Board] -> Char -> [Board]
generateNewMoves history side =
     (concat 
          [(generateRightMoves history side 0 []),
          (generateLeftMoves history side 0 []),
          (generateUpLeftMoves history side 0 []),
          (generateUpRightMoves history side 0 []),
          (generateDownRightMoves history side 0 []),
          (generateDownLeftMoves history side 0 [])])


-- Takes in a Board with length n and reverses the orientation of the asterisks
-- for UR and DL movements.
swapBoardAsterisks :: Board -> Board
swapBoardAsterisks initBoard =
     reverse (addOutOfBounds 
           (reverse (revertBoard (map (reverse) initBoard))) 
       n 1 [])
      where n = getN initBoard

-- Takes in a swapped Board and sets it back to a regular Board
unswapBoardAsterisks :: Board -> Board
unswapBoardAsterisks swappedBoard =
     (addOutOfBounds 
           (revertBoard swappedBoard) 
       n 1 [])
      where n = getN swappedBoard

getN :: Board -> Int
getN board = quot (length board + 1) 2


generateDownLeftMoves :: [Board] -> Char -> Int -> [Board] -> [Board]
generateDownLeftMoves history side index acc =
   (map unswapBoardAsterisks
        (generateDownRightMoves (map swapBoardAsterisks history) side index acc))

generateUpRightMoves :: [Board] -> Char -> Int -> [Board] -> [Board]
generateUpRightMoves history side index acc =
  (map unswapBoardAsterisks 
       (generateUpLeftMoves (map swapBoardAsterisks history) side index acc))


generateUpLeftMoves :: [Board] -> Char -> Int -> [Board] -> [Board]
generateUpLeftMoves history side index acc = 
     (map transpose
          (generateLeftMoves (map transpose history) side index acc))

generateDownRightMoves :: [Board] -> Char -> Int -> [Board] -> [Board] 
generateDownRightMoves history side index acc = 
      (map transpose
          (generateRightMoves (map transpose history) side index acc))


-- level starts at 0 and accumlator starts at empty
-- returns all the possible Boards that can be created by moving
-- each pawn of the given side in initBoard to the RIGHT.
-- Looks at each row, keep track of what number row we're on.
-- If it has at least one side pawn, generates states from it.
generateLeftMoves :: [Board] -> Char -> Int -> [Board] -> [Board]
generateLeftMoves history side index acc = (map reverseBoard 
                                          (generateRightMoves (map reverseBoard history) side index acc))


reverseBoard :: Board -> Board
reverseBoard board = map reverse board

{- ********** MOVE RIGHT FUNCTIONS ************** -}

-- level starts at 0 and accumulator starts at empty
-- Returns all the possible Boards that can be created by moving
-- each pawn of the given side in initBoard to the RIGHT.
-- Looks at each row, keep track of what number row we're on. 
-- If it has at least one side pawn, generate states from it.

generateRightMoves :: [Board] -> Char -> Int -> [Board] -> [Board]
generateRightMoves history side level acc
     | level == (length initBoard)             = getNotInHistory history acc
     | elem side (initBoard !! level)          = (generateRightMoves history side (level + 1)
                                               ((createBoardsFromRightMoves
                                                initBoard
                                                level
                                                (generateRightMovesFromRow (initBoard !! level) side 0 []))
                                                ++ acc)
                                               )

     | otherwise                            = (generateRightMoves
                                               history
                                               side
                                               (level + 1)
                                               acc)
     where initBoard = (head history)


-- removes any generated new states that are already in the history
getNotInHistory :: [Board] -> [Board] -> [Board]
getNotInHistory history newstates
  | null history       = newstates
  | otherwise          = getNotInHistory
          (tail history)
          (filter (/= (head history)) newstates)


-- takes a list of rows and creates many possibilities with row it is being replaced with
-- rows is the list of rows from the generated right moves function
createBoardsFromRightMoves :: Board -> Int -> [Row] -> [Board]
createBoardsFromRightMoves initBoard level rows 
    | null rows              = []
    | otherwise              = (createBoardFromRightMove initBoard (head rows) [] level)
                              :(createBoardsFromRightMoves initBoard level (tail rows))



-- takes a single row and creates a new board with the row it is being replaced with
createBoardFromRightMove :: Board -> Row -> [Row] -> Int -> Board
createBoardFromRightMove initBoard row acc level 
  | null initBoard              = reverse acc
  | level == 0                  = (createBoardFromRightMove (tail initBoard) row (row:acc) (level-1))
  | otherwise                   = (createBoardFromRightMove (tail initBoard) row ((head initBoard):acc) (level-1))


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
    | (canMoveRight realRow)                    = (headXOfString_o7m8 row index) ++ (moveRight_helper realRow) 
    | (canJumpRight realRow)                   = (headXOfString_o7m8 row index) ++ (jumpRight_helper realRow)
    | otherwise                                 = []
        where realRow = (tailXOfString_o7m8 row index)

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
       			    (newBotRow_o7m8 initstring n index))
      where recurse newIndex newRow = addOutOfBounds 
       	       		      	      (tailXOfString_o7m8 initstring newIndex) 
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
       	     		    (tailXOfString_o7m8 initstring (n+index-1)) 
       	       		    n 
			    (index+1) 
       	       		    ((newTopRow initstring n index):rows)
     | otherwise 	  = addOutOfBounds
       	     		    (tailXOfString_o7m8 initstring ((3*n)-index-1)) 
       	       		    n 
			    (index+1) 
       	       		    ((newBotRow_o7m8 initstring n index):rows)



-- Creates a new top row of the board with asterisks added
-- Note that (n - index) + (n + index - 1) = 2n-1
newTopRow :: String -> Int -> Int -> Row
newTopRow initstring n index =
     (makeOOB_o7m8 (n - index)) ++ 
     (headXOfString_o7m8 initstring (n + index - 1))

-- Creates a new bottom row of the board with asterisks added
-- Note that (3n - index - 1) + (index - n) = 2n-1
newBotRow_o7m8 :: String -> Int -> Int -> Row
newBotRow_o7m8 initstring n index =
     (headXOfString_o7m8 initstring ((3*n) - index - 1)) ++
     (makeOOB_o7m8 (index - n))

-- Gets the first x elements of a String
headXOfString_o7m8 :: String -> Int -> String
headXOfString_o7m8 init x =
     (fst (splitAt x init))

-- Gets the last elements after index x
tailXOfString_o7m8 :: String -> Int -> String
tailXOfString_o7m8 init x =
     (snd (splitAt x init))

-- make String of outOfBounds with length x 
makeOOB_o7m8 :: Int -> String
makeOOB_o7m8 x =
     replicate x outOfBounds

-- ********************************************

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

