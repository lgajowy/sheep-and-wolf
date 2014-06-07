module Board where

import Piece
import Data.List
import Utils

-- module that stores all necesary functions for game board logic

data Square = EmptySquare | Square (Maybe Piece)

instance Show Square where
    show EmptySquare                = "_"
    show (Square Nothing )          = " "
    show (Square (Just piece) )     = show piece

instance Read Square where
readsPrec _ (s:xs)  | s == '_' = [(EmptySquare, xs)]
                    | s == ' ' = [(Square Nothing, xs)]
                    | s == 'W' = [(Square (Just Wolf), xs)]
                    | s == 'S' = [(Square (Just Sheep), xs)]

type Board = [[Square]]

printRow :: [Square] -> IO ()
printRow row = do putStrLn (foldl (++) [] (map show row))

printBoard :: Board -> IO ()
printBoard []     = error "Trying to print empty board (0 x 0)"
printBoard [x]    = do printRow x
                       putStrLn "\n"
printBoard (x:xs) = do printRow x
                       printBoard xs
 
updateMatrixAt ::  Position -> (Square->Square) -> Board -> Board
updateMatrixAt (j,i) f board
 | (upperRows, thisRow : lowerRows ) <- splitAt i board
 , (leftCells, thisCell: rightCells) <- splitAt j thisRow
         =                  upperRows
          ++ (leftCells ++ (f thisCell): rightCells)
                          : lowerRows
 | otherwise = error "Tried to index matrix outside range"


moveWolfOnBoard oldPosition newPosition board = do
    return (putWolfInSquare (putNothingInSquare board oldPosition) newPosition)

moveSheepOnBoard board oldPositions newPositions =
    return (foldl (putNothingInSquare) (foldl (putSheepInSquare) board (tail (newPositions))) ((tail oldPositions) \\ (tail (newPositions))))

putSheepInSquare board position = updateMatrixAt position (\_ -> Square(Just Sheep)) board
putWolfInSquare board position = updateMatrixAt position (\_ -> Square(Just Wolf)) board
putNothingInSquare board position = updateMatrixAt position (\_ -> Square(Nothing)) board
