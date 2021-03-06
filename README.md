cs312project1
=============

Crusher Game


Need to implement:
			
- Represent board as a list of Strings
** A list of list of characters
	- Take in single initial parameter String and convert to list of Strings to represent our Board.
		- List consists of 2n-1 Strings with each String length 2n-1 
		- Representing our Hexagonal Board as a Square Board like RushHour.
		- Board looks like THIS:
		
		  w w w
		 - w w -
		- - - - -
		 - b b -
		  b b b
		
		* * W W W 
		* - W W - 
		- - - - -
		- B B - *
		B B B * *
			
		- Asterisks represent edge of board. Added to each String so that we can have Strings of equal length in our List of Strings. 	
		- See below how MOVING PAWNS works.
			
- Check if we are at a winning board state. If we are, DONE.
	- The board is at a win if:
		- Our team has captured N enemy pawns (less than N remain)
		- Enemy cannot move. Can't move if: 
			- Physically trapped
			- Forced to recreate history

- For a given board state, generate new possible moves
	- Each pawn can move in 6 directions. L, R, UL, UR, DL, DR
		- L and R are easy enough to do. Move char along same String. 
		- UL, move char to String above (preceding) it in the same column (String index). Could transpose and L...
		- UR, move UL then R.
		- DR, move char to String below. Could transpose and R...
		- DL, move DR then L. 
		**Is there better way?
		
	- Check each of the 6 directions if it's empty.
		- If next to ally, we can jump if NEXT space is:
			- enemy (CRUSH)
			- or empty 
	- Can't move if obstacle in the way. Obstacles include:
		- Edge of Board
		- Enemy Pawns
		- 2 layers of friendly Pawns
			- Check friendly pawn in same direction.
	- Determine whether move is legal before adding it to list of moves to consider.
		- move is illegal if it:
			- does not match a move in the history
	- IF WE CANNOT GENERATE ANY NEW POSSIBLE MOVES, THEN WE HAVE LOST
		- generateNewMoves will account for history moves
		
	- Once we have generated all legal moves, heuristically determine whether a move is good before adding it to list of moves to consider. 
		**For the below lists, the uppermost options have more weight
		** ie. We should be more inclined to move to a winning position than just capture a pawn. Our opponents will do the same.
	- We will be LOOKING AHEAD several moves of ours and our opponent with the MINMAX algorithm. 
	- A move is good if it:
		(compared to opponent)
		- Doesn't put us in a worse position. A worse position is:
			- We lose on the opponent's move 				
			- We lose a pawn on the opponent's move
			- *experimental*
		(compared to us)	
		- Puts us in a better position.
			- Puts us in win state
			- We capture an enemy pawn
			- *experimental* We move pawns closer / set up capture
			- *experimental* We move closer to the middle of the board			
	
	- STATIC BOARD EVALUATOR
		- Looks at a board and assigns it a value calculated based on our above heuristic.
		- DOES NOT LOOK AHEAD MOVES. Moves already in history don't get considered by board evaluator.
		- Scoring is based on given board and side and is as follows:
			- If board is at crusher win, assign 100 points and done.
			- Alternatively, if it is at crusher loss, assign -100 points and be done with it.
			- Otherwise, count the number of our pawns and opponents pawns. Multiply each by 10 and subtract our score from their score. (EX: We have 4 pawns, they have 3 so 40 - 30 = +10)
			- *experimental* For each pawn that's adjacent to a friendly pawn, add 5.
			- *experimental* For each pawn in the "centre" of the board, add 5.
		