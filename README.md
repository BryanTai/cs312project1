cs312project1
=============

Crusher Game


Need to implement:
			
- Check if we are at a winning board state. If we are, DONE.
	- The board is at a win if:
		- Our team has captured N enemy pawns (less than N remain)
		- Enemy cannot move. Can't move if: 
			- Physically trapped
			- Forced to recreate history

- For a given board state, generate new possible moves
	- Each pawn can move in 6 directions.
		- Check each of the 6 directions if it's empty.
		- If next to ally, we can jump if NEXT space is:
			- enemy (CRUSH)
			- or empty 
	- Can't move if obstacle in the way. Obstacles include:
		- Edge of Board
		- Enemy Pawns
		- 2 layers of friendly Pawns
	
	- Determine whether move is legal before adding it to list of moves to consider.
		- move is illegal if it:
			- does not match a move in the history
	- Heuristically determine whether a move is good before adding it to list of moves to consider. 
		**For these lists, the uppermost options have more weight
		** ie. We should be more inclined to move to a winning position than just capture a pawn. Our opponents will do the same.
	- We will be looking ahead several moves of ours and our opponent with the MINMAX algorithm. A move is good if it:
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
		