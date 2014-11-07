-- CPSC312 Project 1
-- Bryan Tai and Linh Phan
-- o7m8 and a5i8


import Data.List
 
-- constants
outOfBounds	    = '*'
blackPawn 	    = 'B'
whitePawn 	    = 'W'
emptySpace	    = '-'

-- Board Representation:
--
--   * * W W W 
--   * - W W - 
--   - - - - -
--   - B B - *
--   B B B * *

-- This will make our code easier to read
type Row   = String
type Board = [Row]


----TODO

-- Takes in a Board history, calculates the best next move using
-- our heuristic scores and the MINMAX algorithm and
-- returns an updated Board history with our new move at the head
-- also if inital Board is gameOver, return the last move again.
crusher_o7m8 :: [String] -> Char -> Int -> Int -> [String]
crusher_o7m8 history side searchDepth n 
    | gameOver_o7m8 (head history) n  = (head history):history
    | otherwise	    	  	      = nextMove:history 
     where nextMove = revertBoard_o7m8 (findNextMove_o7m8 
     	   	      		       (initializeBoards_o7m8 n history)
				       side
				       searchDepth
				       n
				       True)


-- Generates the BEST NEXT BOARD from the given initial Board (and history)
-- Chooses from the first new moves generated from the last move
-- Calculates the best heuristic score for each of these moves 
-- using recursive calls to stateSearch.
-- findMax indicates whether a level assigned MAX or MIN, initially TRUE.
-- Alternates each recursive call

findNextMove_o7m8 :: [Board] -> Char -> Int -> Int -> Bool -> Board
findNextMove_o7m8 history side searchDepth n findMax =
     (getMaxBoard_o7m8 history 
     		       side 
		       searchDepth 
		       n 
		       findMax 
		       (generateNewMoves_o7m8 history side) 
		       (head history) 
		       (-1001))
     
			   
-- Takes in the list of possible next moves, calls stateSearch on each
-- to find the Board with the maximum heuristic score and returns it. 
-- maxBoard and maxInt keep track of board with current best heuristic score
-- initial values of maxBoard and maxInt should change.
getMaxBoard_o7m8 :: [Board] -> Char -> Int -> Int -> Bool -> [Board] -> Board -> Int-> Board
getMaxBoard_o7m8 history side searchDepth n findMax genBoards maxBoard maxInt
     | null genBoards          = maxBoard
     | (score (head genBoards)) > maxInt 
       	      = getMaxBoard_o7m8 history 
	      			 side 
				 searchDepth 
				 n 
				 findMax 
				 (tail genBoards) 
				 (head genBoards) 
				 (score (head genBoards))
     | otherwise    		  	 
       	      = getMaxBoard_o7m8 history 
	      			 side 
				 searchDepth 
				 n 
				 findMax 
				 (tail genBoards) 
				 maxBoard 
				 maxInt
     where score = (stateSearch_o7m8 history side (searchDepth-1) n (not findMax)) 

-- Given an initBoard, generates next moves until searchDepth is reached
-- then calculates the heuristic scores of the last generated boards
-- and propagates the best ones up (determined by MINMAX) using recursion. 
-- If searchDepth is reached (leaf node), return score of initBoard
-- Else, generate new moves for the other side and return the best heuristic
-- score among them. Choose either minimum or maximum depending on value of findMax
stateSearch_o7m8 :: [Board] -> Char -> Int -> Int -> Bool -> Board -> Int
stateSearch_o7m8 history side searchDepth n findMax initBoard
     | (searchDepth == 0) = boardEvaluator_o7m8 side n initBoard 
     | otherwise       	  = getMinMax_o7m8 findMax 
       			    		   (map f (generateNewMoves_o7m8 history side)) 	
   where f = (stateSearch_o7m8 (initBoard:history)
     		    	       (otherSide_o7m8 side) 
		    	       (searchDepth-1)
		    	       n
		    	       (not findMax))


{- ********** STATIC BOARD EVALUATOR FUNCTIONS ************** -}

-- Static Board Evaluator
-- Takes in a Board and relevant information. Returns a score based
-- on our heuristic.
-- Cannot look ahead moves (ie. generate new states)
-- Will not be passed a board that has already been seen by history
-- Whoever calls this function should take care of that maybe
-- <post heuristic here>
boardEvaluator_o7m8 :: Char -> Int -> Board -> Int
boardEvaluator_o7m8 side n initBoard 
     | (isCrushedWin_o7m8 initBoard side n)		   =  1000
     | (isCrushedWin_o7m8 initBoard (otherSide_o7m8 side) n) = -1000
     | otherwise     	= calculateScore_o7m8 initBoard side 

-- Calculates the score of a Board that has not won nor lost.
-- (# of given side's pawns times 10) minus (enemy pawns times 10)
--
-- TODO check for adjacent pawns
-- TODO check if pawns in middle / edge
calculateScore_o7m8 :: Board -> Char -> Int
calculateScore_o7m8 initBoard side =
    ((getPawnsForSide_o7m8 initBoard side) * 10) -
    ((getPawnsForSide_o7m8 initBoard (otherSide_o7m8 side)) * 10)

-- Given a Board, returns the number of Pawns of the given side.
getPawnsForSide_o7m8 :: Board -> Char -> Int
getPawnsForSide_o7m8 initBoard side =
     (length (filter (side ==)(revertBoard_o7m8 initBoard)))

-- If the # of enemy pawns is less than n, the given side wins the Board.
isCrushedWin_o7m8 :: Board -> Char -> Int -> Bool
isCrushedWin_o7m8 initBoard side n 
     = n > (getPawnsForSide_o7m8 initBoard (otherSide_o7m8 side))

-- Given a pawn colour, simply returns the character of the other team. 
-- Input is either 'W' or 'B'
otherSide_o7m8 :: Char -> Char
otherSide_o7m8 side 
     | side == whitePawn	= blackPawn
     | otherwise 		= whitePawn

--Curried function
--Evaluates the maximum or minimum of next Int list depending on given boolean
getMinMax_o7m8 :: Bool -> [Int] -> Int
getMinMax_o7m8 findMax 
     | findMax   = maximum 
     | otherwise = minimum

-- Checks whether a Board String is already over due to one side having
-- less than n pawns
gameOver_o7m8 :: String -> Int -> Bool
gameOver_o7m8 initString n = 
     (isCrushedWin_o7m8 initBoard whitePawn n) ||
     (isCrushedWin_o7m8 initBoard blackPawn n)
     where initBoard = (initializeBoard_o7m8 n initString)


{- ********** MOVE GENERATION FUNCTIONS ************** -}

-- Takes in a Crusher Board and the current history 
-- and returns the all the next possible Crusher Boards.
-- If NO moves returned, then the given Crusher board is in a position
-- where no moves are possible for the given side (thus, they lose)
generateNewMoves_o7m8 :: [Board] -> Char -> [Board]
generateNewMoves_o7m8 history side =
     (concat 
          [(generateRightMoves_o7m8 history side 0 []),
          (generateLeftMoves_o7m8 history side 0 []),
          (generateUpLeftMoves_o7m8 history side 0 []),
          (generateUpRightMoves_o7m8 history side 0 []),
          (generateDownRightMoves_o7m8 history side 0 []),
          (generateDownLeftMoves_o7m8 history side 0 [])])

-- All generated moves revolve around moving to the RIGHT.
-- Boards are transformed before being fed into generateRightMoves
-- and the resulting boards are transformed back.

-- EX) To generateLeftMoves, reverse the Rows in each board first.

generateLeftMoves_o7m8 :: [Board] -> Char -> Int -> [Board] -> [Board]
generateLeftMoves_o7m8 history side index acc = (map reverseBoard_o7m8 
                                          (generateRightMoves_o7m8 (map reverseBoard_o7m8 history) side index acc))

-- Reverses all the Rows in a Board
reverseBoard_o7m8 :: Board -> Board
reverseBoard_o7m8 board = map reverse board

-- To move a pawn UpLeft or DownRight on a hexagonal board, 
-- move the pawn up or down on our Square Board representation.

generateUpLeftMoves_o7m8 :: [Board] -> Char -> Int -> [Board] -> [Board]
generateUpLeftMoves_o7m8 history side index acc = 
     (map transpose
          (generateLeftMoves_o7m8 (map transpose history) side index acc))

generateDownRightMoves_o7m8 :: [Board] -> Char -> Int -> [Board] -> [Board] 
generateDownRightMoves_o7m8 history side index acc = 
      (map transpose
          (generateRightMoves_o7m8 (map transpose history) side index acc))

-- To move a pawn UpRight or DownLeft, swap the asterisks in our Square Board
-- representation from left to right.

generateDownLeftMoves_o7m8 :: [Board] -> Char -> Int -> [Board] -> [Board]
generateDownLeftMoves_o7m8 history side index acc =
   (map unswapBoardAsterisks_o7m8
        (generateDownRightMoves_o7m8 (map swapBoardAsterisks_o7m8 history) side index acc))

generateUpRightMoves_o7m8 :: [Board] -> Char -> Int -> [Board] -> [Board]
generateUpRightMoves_o7m8 history side index acc =
  (map unswapBoardAsterisks_o7m8 
       (generateUpLeftMoves_o7m8 (map swapBoardAsterisks_o7m8 history) side index acc))

-- Takes in a Board with length n and reverses the orientation of the 
-- asterisks from left to right. Used for UR and DL movements.
swapBoardAsterisks_o7m8 :: Board -> Board
swapBoardAsterisks_o7m8 initBoard =
     reverse (addOutOfBounds_o7m8 
           (reverse (revertBoard_o7m8 (map (reverse) initBoard))) 
       n 1 [])
      where n = getN_o7m8 initBoard

-- Takes in an asterisk swapped Board and sets it back to a regular Board
unswapBoardAsterisks_o7m8 :: Board -> Board
unswapBoardAsterisks_o7m8 swappedBoard =
     (addOutOfBounds_o7m8 
           (revertBoard_o7m8 swappedBoard) 
       n 1 [])
      where n = getN_o7m8 swappedBoard

-- Given a Board, finds the original value of n
getN_o7m8 :: Board -> Int
getN_o7m8 board = quot (length board + 1) 2		  
		  

{- ********** MOVE RIGHT FUNCTIONS ************** -}

-- Given an initial Board, returns all the possible Boards that 
-- can be created by moving each pawn of the given side to the RIGHT. 
-- Looks at each row, keep track of what level we're on. 
-- If the row has at least one side pawn, generate new rows from it
-- and create new Boards with the new rows inserted.
-- level starts at 0 and accumulator starts at empty

generateRightMoves_o7m8 :: [Board] -> Char -> Int -> [Board] -> [Board]
generateRightMoves_o7m8 history side level acc
     | level == (length initBoard)     = getNotInHistory_o7m8 history acc
     | elem side (initBoard !! level)  
          = (generateRightMoves_o7m8 history side (level + 1)
                           ((createBoardsFromRightMoves_o7m8
                                 initBoard
                                 level
                                 (generateRightMoves_o7m8FromRow_o7m8 
				 		(initBoard !! level) side 0 []))
                                                ++ acc))
                                              
     | otherwise                            = (generateRightMoves_o7m8
                                               history
                                               side
                                               (level + 1)
                                               acc)
     where initBoard = (head history)


-- Removes any generated new states that are already in the history
getNotInHistory_o7m8 :: [Board] -> [Board] -> [Board]
getNotInHistory_o7m8 history newstates
  | null history       = newstates
  | otherwise          = getNotInHistory_o7m8
          (tail history)
          (filter (/= (head history)) newstates)

-- takes a list of rows and creates many possibilities with row it is being replaced with
-- rows is the list of rows from the generated right moves function

-- Takes in an initialBoard, a list of generatedRows, and a Board level.
-- Creates a new Board for every Row in genRows where each new 
createBoardsFromRightMoves_o7m8 :: Board -> Int -> [Row] -> [Board]
createBoardsFromRightMoves_o7m8 initBoard level genRows 
    | null genRows   = []
    | otherwise      = (createBoardFromRightMove_o7m8 initBoard (head genRows) [] level)
                      :(createBoardsFromRightMoves_o7m8 initBoard level (tail genRows))



-- takes a single Board and replaces the Row at the given level with the new given Row 
createBoardFromRightMove_o7m8 :: Board -> Row -> [Row] -> Int -> Board
createBoardFromRightMove_o7m8 initBoard row acc level 
  | null initBoard  = reverse acc
  | level == 0      = (createBoardFromRightMove_o7m8 (tail initBoard) row (row:acc) (level-1))
  | otherwise       = (createBoardFromRightMove_o7m8 (tail initBoard) row ((head initBoard):acc) (level-1))


-- For the given row, check each Char. 
-- If it matches side, check if it can move right
-- if it can, move it and add the new generated Rows to acc 
-- if not, recursive call to the next char
-- NOTE that acc and the return value are NOT Boards. Just a list of possible Rows.
-- intial index starts at 0
generateRightMoves_o7m8FromRow_o7m8 :: Row -> Char -> Int -> [Row] -> [Row]
generateRightMoves_o7m8FromRow_o7m8 row side index acc 
    | index  == (length row)    = filter (/= "") acc
    | (row !! index) == side    = (generateRightMoves_o7m8FromRow_o7m8 
                                                    row 
                                                    side 
                                                    (index + 1) 
                                                    ((moveRight_o7m8 row index) : acc))
    | otherwise                 = (generateRightMoves_o7m8FromRow_o7m8 
                                                    row
                                                    side
                                                    (index + 1)
                                                    acc)


-- For the given row, shifts or jumps the element at the index to the right if it is a legal move.
-- Returns "" if the move is not legal.
moveRight_o7m8 :: Row -> Int -> Row
moveRight_o7m8 row index
    | (canMoveRight_o7m8 realRow)  = (headXOfString_o7m8 row index) ++ (moveRight_o7m8_helper realRow) 
    | (canJumpRight_o7m8 realRow)  = (headXOfString_o7m8 row index) ++ (jumpRight_helper_o7m8 realRow)
    | otherwise                    = []
        where realRow = (tailXOfString_o7m8 row index)

-- takes in a partial row and moves the first element to the right by 1
-- requires that the given row can be moved right
moveRight_o7m8_helper :: Row -> Row
moveRight_o7m8_helper row = emptySpace:(head row):(tail (tail row))

-- Jumps an element from a row over another element 
-- requires that the given row can be jumped right
jumpRight_helper_o7m8 :: Row -> Row
jumpRight_helper_o7m8 row = (emptySpace:(head (tail row)):(head row):[]) ++ (tail (tail (tail row)))

-- Takes in a segment of a Row and checks whether the head character
-- can jump right 2 spaces.
-- head char should be either whitePawn or blackPawn
-- It can jump right only if the spot 2 spaces away is NOT ally or out of bounds
canJumpRight_o7m8 :: Row -> Bool
canJumpRight_o7m8 row 
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
canMoveRight_o7m8 :: Row -> Bool
canMoveRight_o7m8 row
     | null row || null (tail row)	  = False
     | otherwise  	      	          = (head (tail row)) == emptySpace


{- ********** BOARD STATE FUNCTIONS ************** -}

-- Takes in a single String that represents a hexagonal Crusher board
-- with side length n and converts it into our Square Board representation
-- with asterisks included to represent empty spaces.
-- EXAMPLE)
-- INPUT
-- "WWW-WW-------BB-BBB"
-- OUTPUT
-- ["**WWW","*-WW-","-----","-BB-*","BBB**"]
--
--   * * W W W 
--   * - W W - 
--   - - - - -
--   - B B - *
--   B B B * *

initializeBoard_o7m8 :: Int -> String -> Board
initializeBoard_o7m8 n initstring =
     addOutOfBounds_o7m8 initstring n 1 []

-- Initializes a whole list of Strings into a list of Boards.
initializeBoards_o7m8 :: Int -> [String] -> [Board]
initializeBoards_o7m8 n initstrings = 
     map f initstrings
     where f = initializeBoard_o7m8 n

-- Convert a list of our square Boards back to original String form.
-- Basically, the opposite of initializeBoards_o7m8
revertBoards_o7m8 :: [Board] -> [String]
revertBoards_o7m8 boards =
     map revertBoard_o7m8 boards

-- takes a single Square Board and turns it back to original String form
revertBoard_o7m8 :: Board -> String
revertBoard_o7m8 board =
     filter (/=outOfBounds) (concat board) 

-- Takes in a String representing a Board with side length n.
-- Splits it up and returns a Board with asterisks added to 
-- represent the outOfBounds areas.
-- Initial index is 1, increases until it is 2n-1
-- Don't worry about the math. I did all the math. It works. 
addOutOfBounds_o7m8 :: String -> Int -> Int -> [Row] -> Board
addOutOfBounds_o7m8 initstring n index rows
     | null initstring    = (reverse rows)
     | index <= n      	  = addOutOfBounds_o7m8
       	     		    (tailXOfString_o7m8 initstring (n+index-1)) 
       	       		    n 
			    (index+1) 
       	       		    ((newTopRow_o7m8 initstring n index):rows)
     | otherwise 	  = addOutOfBounds_o7m8
       	     		    (tailXOfString_o7m8 initstring ((3*n)-index-1)) 
       	       		    n 
			    (index+1) 
       	       		    ((newBotRow_o7m8 initstring n index):rows)



-- Creates a new top row of the board with asterisks added
-- Asterisks on the left, Board spaces on the right
-- EX) "**" ++ "-WB"
-- Note that (n - index) + (n + index - 1) = 2n-1
newTopRow_o7m8 :: String -> Int -> Int -> Row
newTopRow_o7m8 initstring n index =
     (makeOOB_o7m8 (n - index)) ++ 
     (headXOfString_o7m8 initstring (n + index - 1))

-- Creates a new bottom row of the board with asterisks added
-- Board spaces on the left, Asterisks on the right
-- EX) "-WB" ++ "**"
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

{- ********** MANUAL TEST FUNCTIONS ************** -}

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

printStrs :: [String] -> Int -> IO ()
printStrs strings n =
     printStrMatrix (initializeBoards_o7m8 n strings)


--Test Values

testString3n = "WWW-WW-------BB-BBB"
testAsteriskString3n = "**WWW*-WW-------BB-*BBB**"
testBoard3n = ["**WWW","*-WW-","-----","-BB-*","BBB**"]

testString3nFirst = "-WW-WW---W---BB-BBB"
testBoard3nFirst  = ["**-WW","*-WW-","--W--","-BB-*","BBB**"]

testBoard3nWhite = ["**WWW","*-WW-","-----","----*","BB-**"]
testBoard3nEmpty = ["**--","*--B-","--W--","----*","---**"]

testString4n = "WWWW-WWW---------------------BBB-BBBB"

testListOfBoard3n = [testBoard3n, testBoard3nFirst]
testListOfString3n = [testString3nFirst, testString3n]


