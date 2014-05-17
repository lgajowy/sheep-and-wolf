-- Board definitions and basic functions. Like printing.

import System.IO


type Board      = [[Square]]
data Square     = EmptySquare |  Square (Maybe Piece)

data Piece      = Wolf | Sheep

instance Show Piece where
    show Wolf   = "W"
    show Sheep   = "S"
    
instance Read Piece where
    readsPrec _ ('W':xs)   = [(Wolf, xs)]
    readsPrec _ ('S':xs)   = [(Sheep, xs)]


instance Show Square where
    show EmptySquare             = "."
    show (Square Nothing )       = " "
    show (Square (Just p) )      = show p
    

instance Read Square where
    readsPrec _ (s:xs)  | s == '.' = [(EmptySquare, xs)]
                        | s == ' ' = [(Square Nothing, xs)]
                        | s == 'W' = [(Square (Just Wolf), xs)]
                        | s == 'S' = [(Square (Just Sheep), xs)]
    
printRow :: [Square] -> IO ()
printRow row = do putStrLn (foldl (++) [] (map show row))

printBoard :: Board -> IO ()
printBoard [x]    = do printRow x
printBoard (x:xs) = do printRow x
                       printBoard xs
                       

initialBoard :: Board
initialBoard = [
                [EmptySquare, Square (Just Sheep), EmptySquare, Square (Just Sheep), EmptySquare, Square (Just Sheep), EmptySquare, Square (Just Sheep)],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare],
                [EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare],
                [EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare],
                [EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare]
               ]
               
-- IO operations

type GameState = (Board, WSPlayer)
load :: (Read a) => FilePath -> IO a
load f = do s <- readFile f
            return (read s)

save :: Board -> FilePath -> IO ()
save x f = writeFile f (show x)
--Moving Pieces
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
outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7

inside = not . outside

--Player definitions
class Player a where
    next :: a -> a
    
data WSPlayer = WolfPlayer | SheepPlayer deriving (Show,Read)
instance Player WSPlayer where
    next WolfPlayer    = SheepPlayer
    next SheepPlayer   = WolfPlayer

