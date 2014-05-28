module Gametree.GameTree where

import Data.List

-- Our modules
import Gametree.Utils
import Gametree.Moves


data PositionsVerdict = NotEnd | WolfWon | SheepsWon

verdict :: FiguresPositions -> Turn -> PositionsVerdict

--Wolf wins when reaches one of the four top positions
verdictW positions = foldl (&&) True (map (greater (head positions)) (tail positions))
  where greater (_, w) (_, s) = (w <= s)

--Sheeps win when wolf has no move.
--Wolf wins when sheeps have no move in their turn
verdict positions t | verdictW positions = WolfWon
                    | (possibleWolfMoves positions) == [] = SheepsWon
                    | (possibleSheepsMoves positions) == [] && t ==SheepsTurn = WolfWon
                    | otherwise = NotEnd

data Turn = WolfTurn | SheepsTurn deriving (Show, Eq)
data GameTree = Node Turn FiguresPositions deriving Show

--functions for creating the next level of tree
initWolf, initSheeps :: FiguresPositions -> [GameTree]
initWolf a   = map (\x -> Node SheepsTurn x) (possibleWolfMoves a)
initSheeps a = map (\x -> Node WolfTurn x) (possibleSheepsMoves a)

distanceSum :: FiguresPositions -> Int
distanceSum (w:xs) = maximum (map distance (permutations xs)) + (distance (w:xs))

distance :: FiguresPositions -> Int
distance positions = maximum (map (oneDistance (head positions)) (tail positions))
    where oneDistance (x1, y1) (x2, y2) = round (sqrt (fromIntegral((x2 - x1))^2 + fromIntegral((y2 - y1))^2))


--Rates the current node. The tree is evaluated up to 7 level deep. Starting from the given level
--For not fully evaluated nodes it uses heuristic so that the algorithm tries to minimize distance to wolf
-- and distance between sheeps
rate :: GameTree -> Int -> Int
rate (Node t a) depth = if depth < 6 then verd else incompleteVerd
                                    where verd = case verdict a t of
                                                    NotEnd ->  if t == WolfTurn then minimum (map (\x -> rate x (depth + 1)) (initWolf a))
                                                                            else maximum (map (\x -> rate x (depth + 1)) (initSheeps a))
                                                    WolfWon -> (-100)
                                                    SheepsWon -> 100
                                          incompleteVerd = (-1) * (distanceSum a)

--scores the given node.
score :: GameTree -> (FiguresPositions, Int)
score (Node t a) = (a, (rate (Node t a) 1))

-- chooses the best sheeps move in given situation
chooseMove :: FiguresPositions -> FiguresPositions
chooseMove positions = fst (maximumBy (\(x, y) (x1, y1) -> compare y y1) nodes)
            where nodes = map (score) (initSheeps positions)

examplePosition = [(1,6),(0,5),(3,5),(3,7),(5,7)] :: FiguresPositions
lostTree = Node SheepsTurn [(0,7), (0,5), (2,5), (3,6), (4,7)]
exampleTree = Node WolfTurn [(0,0), (0,0), (0,0), (0,0), (0,0)]
gameTree = Node SheepsTurn [(0,7), (1,0), (3,0), (5,0), (7,0)]
