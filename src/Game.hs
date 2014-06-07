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
import UserInteraction

-- main file storing funcions necessary to play the game.

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

initialSheepPositions = [(1,0), (3,0), (5,0), (7,0)] :: [(Int, Int)]

-- runs the game.
run = do
    putStrLn welcomeMsg
    promptAndExecuteOption initialBoard

promptAndExecuteOption gameBoard = do
    putStrLn optionsMsg
    option <- getLine
    executeOption option gameBoard


executeOption option gameBoard = case option of
        "1" -> startGame initialBoard
        "2" -> saveGame gameBoard
        "3" -> loadGame
        "4" -> exitGame
        _ -> do
            putStrLn wrongOptionMsg
            option <- getLine
            executeOption option gameBoard


startGame gameBoard = do
    startingPawnPositions <- chooseWolfStartingPosition initialSheepPositions 
    startingBoard <- moveWolfOnBoard (2,7) (head startingPawnPositions) gameBoard
    printBoard startingBoard
    iterateGame startingBoard startingPawnPositions


chooseWolfStartingPosition sheepPostions = do
    putStrLn wolfStartingPosMsg  
    chosenPosition <- getStartingPositionFromUser
    pawnPos <- return  (chosenPosition : sheepPostions)
    return pawnPos


getStartingPositionFromUser = do
  position <- getLine
  case position of
    "1" -> return (0,7)
    "2" -> return (2,7)
    "3" -> return (4,7)
    "4" -> return (6,7)
    _   -> do 
          putStrLn invalidStartingPositionMsg
          getStartingPositionFromUser


iterateGame gameBoard pawnPositions = do
    displayUserOptions
    option <- getLine
    executeIngameOption option gameBoard pawnPositions


executeIngameOption option gameBoard pawnPositions = case option of
        "1" -> startGame initialBoard
        "2" -> saveGame gameBoard
        "3" -> loadGame
        "4" -> exitGame
        "5" -> do 
                newWolfPosition <- getWolfMovementDirectionFromUser (head pawnPositions) pawnPositions
                wolfMove gameBoard pawnPositions newWolfPosition
        _   -> do
                putStrLn wrongOptionMsg
                option <- getLine
                executeIngameOption option gameBoard pawnPositions


wolfMove gameBoard pawnPositions moveCoordinates = do
    putStrLn wolfMoveMsg
    board <- moveWolfOnBoard (head pawnPositions) moveCoordinates gameBoard
    printBoard board
    newPawnPositions <- return (moveCoordinates : (tail pawnPositions))
    checkVerdict board newPawnPositions WolfTurn

sheepMove gameBoard pawnPositions = do
    putStrLn sheepMoveMsg
    positions <- getNewSheepPositions (pawnPositions)
    board <- moveSheepOnBoard gameBoard (pawnPositions) positions
    printBoard board
    checkVerdict board positions SheepsTurn

checkVerdict board positions turn = case verdict positions turn of
    WolfWon     ->    do 
                        putStrLn wolfWonMsg
                        promptAndExecuteOption board
    SheepsWon   ->    do 
                        putStrLn sheepWonMsg
                        promptAndExecuteOption board
    NotEnd      ->    if turn == SheepsTurn then iterateGame board positions
                                else sheepMove board positions

