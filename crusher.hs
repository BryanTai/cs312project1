-- CPSC312 Project 1
-- Bryan Tai and Linh Phan
 


-- function creates a board that takes in strings and an integer and splits accordingling by breaking
-- it into sublists of strings where each string will represent a row on the board 

board :: String -> Int -> [String]
board strings n
	| (length strings) = 0					= []
	| otherwise								= (splitAt (n strings))        -- creates a tuple



	    * * W W W 
		* - W W - 
		- - - - -
		- B B - *
		B B B * *