module Utils where
-- Utility functions and types

type Position = (Int, Int)

-- edits position so to move by vector
addPosition::(Int,Int)->(Int,Int)->(Int,Int)
addPosition (a,b) (c,d) = (a+c,b+d)

--checks if position is inside the board
outside,inside::Position->Bool
outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7

inside = not . outside

-- cartesian product
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

--represents positions of figures on board
--the first one is the wolf
type FiguresPositions = [Position]
