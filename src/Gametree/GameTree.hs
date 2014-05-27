module Gametree.GameTree where

import Data.List

-- Our modules
import Gametree.Utils
import Gametree.Moves


data PositionsVerdict = NotEnd | WolfWon | SheepsWon

verdict :: FiguresPositions -> Turn -> PositionsVerdict

--Wolf wins when reaches one of the four top positions
verdict [(1,0), _, _, _, _] _ = WolfWon
verdict [(3,0), _, _, _, _] _ = WolfWon
verdict [(5,0), _, _, _, _] _ = WolfWon
verdict [(7,0), _, _, _, _] _ = WolfWon

--Sheeps win when wolf has no move.
--Wolf wins when sheeps have no move in their turn
verdict positions t | (possibleWolfMoves positions) == [] = SheepsWon
                    | (possibleSheepsMoves positions) == [] && t ==SheepsTurn = WolfWon
                    | otherwise = NotEnd

data Turn = WolfTurn | SheepsTurn deriving (Show, Eq)
data GameTree = Node Turn FiguresPositions deriving Show

--functions for creating the next level of tree
initWolf, initSheeps :: FiguresPositions -> [GameTree]
initWolf a   = map (\x -> Node SheepsTurn x) (possibleWolfMoves a)
initSheeps a = map (\x -> Node WolfTurn x) (possibleSheepsMoves a)

--Rates the current node. The tree is evaluated up to 7 level deep. Starting from the given level
--For not fully evaluated nodes it uses heuristic so that the algorithm tries to minimize wolf moves.
--and maximize sheeps moves
rate :: GameTree -> Int -> Int
rate (Node t a) depth = if depth < 7 then verd else if t == WolfTurn then (-1) * length a else (length a)
                                    where verd = case verdict a t of
                                                    NotEnd ->  if t == WolfTurn then minimum (map (\x -> rate x (depth + 1)) (initWolf a)) 
                                                                            else maximum (map (\x -> rate x (depth + 1)) (initSheeps a)) 
                                                    WolfWon -> (-100)
                                                    SheepsWon -> 100
                           
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