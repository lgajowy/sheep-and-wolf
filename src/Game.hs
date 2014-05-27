module Game where

import Board
import Piece
import Player
import IngameDialogs
import IngameOptions
import Gametree.Moves
import Gametree.GameTree
import Gametree.Utils
import Data.List

type GameState = (Board, WSPlayer)

initialBoard :: Board
initialBoard = [
                [EmptySquare, Square (Just Sheep), EmptySquare, Square (Just Sheep), EmptySquare, Square (Just Sheep), EmptySquare, Square (Just Sheep)],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare],
                [EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare],
                [EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing],
                [Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare],
                [EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare, Square Nothing],
                [Square Nothing, EmptySquare, Square(Just Wolf), EmptySquare, Square Nothing, EmptySquare, Square Nothing, EmptySquare]
               ]

initialPawnPositions = [(2,7), (1,0), (3,0), (5,0), (7,0)] :: [(Int, Int)]


run = do
    putStrLn welcomeMsg
    mainProgramLoop

mainProgramLoop = do 
    putStrLn optionsMsg
    option <- getLine
    executeOption option initialBoard
    mainProgramLoop

executeOption option gameBoard = case option of
        "1" -> startGame initialBoard
        "2" -> saveGame gameBoard
        "3" -> loadGame
        "4" -> exitGame
        _ -> do
            putStrLn wrongOptionMsg
            option <- getLine
            executeOption option gameBoard

inGameExecuteOption option gameBoard = case option of
        "5" -> putStrLn "wolf moves\n"  --wolf movement
        _   -> do executeOption option gameBoard
                  putStrLn "wolf moves either\n"  --wolf movement        

startGame gameBoard = do 
    putStrLn "game started"
    gameLoop gameBoard initialPawnPositions

gameLoop gameBoard pawnPositions = do   -- TODO WolfMove. Jezeli gra nie skonczona to sheepmove - zamiast pętli
    displayOptionsAndBoard gameBoard
    option <- getLine
    inGameExecuteOption option gameBoard

    board <- moveWolfOnBoard (head pawnPositions) (3,6) gameBoard -- TODO: pass position
    positions <- getNewSheepPositions ((3,6):(tail pawnPositions))
    board <- moveSheepOnBoard board (pawnPositions) positions
    
    checkVerdict board positions SheepsTurn


checkVerdict board pawnPositions turn = do 
    case verdict pawnPositions turn of
        WolfWon ->      putStrLn "Wolf has won!"
        SheepsWon ->    putStrLn "Sheep has won!"
        NotEnd ->       case turn of 
                            SheepsTurn -> gameLoop board pawnPositions
                            WolfTurn -> putStrLn "Continue."
        

wolfMovement = do
    putStrLn "enter position"

validateWolfPosition newPosition (wolf:sheep) = (newPosition:sheep) `elem` possibleWolfMoves (wolf:sheep)

moveWolfOnBoard oldPosition newPosition board = do
      return (updateMatrixAt newPosition (\_ -> Square(Just Wolf)) (updateMatrixAt oldPosition (\_ -> Square(Nothing)) board))

getNewSheepPositions oldPositions = do
    return (chooseMove oldPositions)

moveSheepOnBoard board oldPositions newPositions =
            return (foldl (putNothing) (foldl (putSheep) board (tail (newPositions))) ((tail oldPositions) \\ (tail (newPositions))))

putSheep board position = updateMatrixAt position (\_ -> Square(Just Sheep)) board

putNothing board position = updateMatrixAt position (\_ -> Square(Nothing)) board

displayOptionsAndBoard board = do
    printBoard board
    putStrLn (optionsMsg ++ moveOptionMsg)