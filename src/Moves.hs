module Moves where

import Data.Maybe
import Utils

--common functions

--checks if all positions in vector are different
allDifferent :: [Position] -> Bool
allDifferent [x] = True
allDifferent (x:xs) = (foldl (&&) True differences) && (allDifferent xs)
        where differences = map (x /=) xs

--Sheeps
getSheepPosition :: FiguresPositions -> Int -> Position
getSheepPosition [ _, x, _, _, _ ] 1 = x 
getSheepPosition [ _, _, x, _, _ ] 2 = x
getSheepPosition [ _, _, _, x, _ ] 3 = x
getSheepPosition [ _, _, _, _, x ] 4 = x

updateSheepPosition :: FiguresPositions -> Int -> Position -> [Position]
updateSheepPosition [ a, _, c, d, e ] 1 b = [ a, b, c, d, e ]
updateSheepPosition [ a, b, _, d, e ] 2 c = [ a, b, c, d, e ]
updateSheepPosition [ a, b, c, _, e ] 3 d = [ a, b, c, d, e ]
updateSheepPosition [ a, b, c, d, _ ] 4 e = [ a, b, c, d, e ]


-- tries to move a sheep, if move valid returns the new vector of positions
-- Nothing otherwise
moveSheep :: FiguresPositions -> Int -> Position -> Maybe FiguresPositions
moveSheep positions n position  | outside sheepPosition = Nothing
                                | allDifferent potentialPositions = Just potentialPositions
                                | otherwise = Nothing
                                where sheepPosition = addPosition (getSheepPosition positions n) position
                                      potentialPositions = updateSheepPosition positions n sheepPosition


possibleSheepsMoves :: FiguresPositions -> [FiguresPositions]
possibleSheepsMoves positions = foldl (++) [] (map (\x -> (mapMaybe (moveSheep positions x) moves)) sheeps)
        where moves = cartProd [-1, 1] [1]
              sheeps = [1..4]


--Wolf
-- tries to move the wolf, if move valid returns the new vector of positions
-- Nothing otherwise
moveWolf :: FiguresPositions -> Position -> Maybe FiguresPositions
moveWolf [a, b, c, d, e] f      | outside wolfPosition = Nothing
                                | allDifferent [wolfPosition, b, c, d, e] = Just [wolfPosition , b, c, d, e]
                                | otherwise = Nothing
                                where wolfPosition = addPosition a f
                                
possibleWolfMoves :: FiguresPositions -> [FiguresPositions]
possibleWolfMoves positions = mapMaybe (moveWolf positions) moves
        where moves = cartProd [-1, 1] [-1, 1]
              
validateWolfPosition newPosition (wolf:sheep) = (newPosition:sheep) `elem` possibleWolfMoves (wolf:sheep)







