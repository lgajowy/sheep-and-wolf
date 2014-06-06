module Board where

import Piece
import Data.List
import Gametree.Utils


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

findWolf :: Board -> Int -> (Int, Maybe Int)
findWolf (headRow:tailRows) counter = if (findWolfInRow(headRow) == Nothing) 
                                    then findWolf tailRows (counter + 1)
                                    else (counter, findWolfInRow(headRow))

findWolfInRow :: [Square] -> Maybe Int
findWolfInRow row = findIndex(fieldHasWolf) row

fieldHasWolf field = 
  case field of
    EmptySquare         -> False
    Square(Just Wolf)   -> True
    _                   -> False

findSheepInRow :: [Square] -> [Int]
findSheepInRow row = findIndices(fieldHasSheep) row

fieldHasSheep field = 
  case field of
    EmptySquare         -> False
    Square(Just Sheep)  -> True
    _                   -> False    

updateMatrixAt ::  Position -> (Square->Square) -> Board -> Board
updateMatrixAt (j,i) f board
 | (upperRows, thisRow : lowerRows ) <- splitAt i board
 , (leftCells, thisCell: rightCells) <- splitAt j thisRow
         =                  upperRows
          ++ (leftCells ++ (f thisCell): rightCells)
                          : lowerRows
 | otherwise = error "Tried to index matrix outside range"


moveWolfOnBoard oldPosition newPosition board = do
      return (updateMatrixAt newPosition (\_ -> Square(Just Wolf)) (updateMatrixAt oldPosition (\_ -> Square(Nothing)) board))

moveSheepOnBoard board oldPositions newPositions =
      return (foldl (putNothing) (foldl (putSheep) board (tail (newPositions))) ((tail oldPositions) \\ (tail (newPositions))))

putSheep board position = updateMatrixAt position (\_ -> Square(Just Sheep)) board
putNothing board position = updateMatrixAt position (\_ -> Square(Nothing)) board
