module Board where

import Piece

data Square = EmptySquare |  Square (Maybe Piece)

instance Show Square where
    show EmptySquare                = "_"
    show (Square Nothing )          = " "
    show (Square (Just piece) )     = show piece

instance Read Square where
readsPrec _ (s:xs)  | s == '.' = [(EmptySquare, xs)]
                    | s == ' ' = [(Square Nothing, xs)]
                    | s == 'W' = [(Square (Just Wolf), xs)]
                    | s == 'S' = [(Square (Just Sheep), xs)]

type Board = [[Square]]

printRow :: [Square] -> IO ()
printRow row = do putStrLn (foldl (++) [] (map show row))

printBoard :: Board -> IO ()
printBoard []     = error "Trying to print empty board (0 x 0)"
printBoard [x]    = do printRow x
printBoard (x:xs) = do printRow x
                       printBoard xs


type Position = (Int, Int)

updateMatrixAt ::  Position -> (Square->Square) -> Board -> Board
updateMatrixAt (i,j) f board
 | (upperRows, thisRow : lowerRows ) <- splitAt i board
 , (leftCells, thisCell: rightCells) <- splitAt j thisRow
         =                  upperRows
          ++ (leftCells ++ (f thisCell): rightCells)
                          : lowerRows
 | otherwise = error "Tried to index matrix outside range"

outside,inside::Position->Bool
outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7       --todo: make it dynamic
inside = not . outside