-- This is why I say Haskell is very expressive [esp. Mathematical expressions & computations]
-- My code for Pascal Triangle in 3 lines 
-- Any language supporting list comprehension expresses this so elegantly
-- Try writing it in X programming language - :P
-- #Haskell #Pascal #Triangle #List #Comprehension #Combinatorics #Fun #Example
module Fun where
	pascal = [ncr_list i | i <- [0..] ]
	       where ncr_list i = [ncr i j | j <- [0..i] ]
	             ncr n r    = product [ (n - r + 1) .. n] `div` product [1..r]



