
module Game where

import Board
import Piece
import Player
import IngameDialogs
import Options
import UserInteraction
import Moves
import GameTree
import Utils


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

initialSheepPositions :: [Position]
initialSheepPositions = [(1,0), (3,0), (5,0), (7,0)] :: [(Int, Int)]

-- runs the game.
run = do
    putStrLn welcomeMsg
    promptAndExecuteOption initialBoard

-- prompts for and executes option
promptAndExecuteOption gameBoard = do
    putStrLn startingOptionsMsg
    option <- getLine
    executeOption option gameBoard

-- puts all pawns on new board
reinitializeBoard pawnPositions = 
    moveSheepOnBoard ( putWolfInSquare initialBoard (head pawnPositions)) initialSheepPositions (tail pawnPositions)

-- runs basic game functions except pawn movement. 
-- this method is used before the game starts and after it finishes
executeOption option gameBoard = case option of
        "1" -> startGame initialBoard
        "2" -> do 
            loadedPawnPositions <- loadGame
            newBoard <- reinitializeBoard loadedPawnPositions
            printBoard newBoard
            iterateGameLoop newBoard loadedPawnPositions
        "3" -> exitGame
        _   -> do
            putStrLn wrongOptionMsg
            option <- getLine
            executeOption option gameBoard

chooseWolfStartingPosition = do
    putStrLn wolfStartingPosMsg  
    chosenPosition <- getStartingPositionFromUser
    return chosenPosition

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

startGame gameBoard = do
    chosenPosition <- chooseWolfStartingPosition
    startingBoard <- return (putWolfInSquare gameBoard (chosenPosition))
    printBoard startingBoard
    iterateGameLoop startingBoard (chosenPosition : initialSheepPositions)

iterateGameLoop gameBoard pawnPositions = do
    displayUserOptions
    option <- getLine
    executeIngameOption option gameBoard pawnPositions

executeIngameOption option gameBoard pawnPositions = case option of
        "1" -> startGame initialBoard
        "2" -> do 
                saveGame pawnPositions
                iterateGameLoop gameBoard pawnPositions
        "3" -> do 
                loadedPawnPositions <- loadGame
                newBoard <- reinitializeBoard loadedPawnPositions
                printBoard newBoard
                iterateGameLoop newBoard loadedPawnPositions
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

-- checks the verdict and performs action adequate to it.
checkVerdict board positions turn = case verdict positions turn of
    WolfWon     ->    do 
                        putStrLn wolfWonMsg
                        promptAndExecuteOption board
    SheepsWon   ->    do 
                        putStrLn sheepWonMsg
                        promptAndExecuteOption board
    NotEnd      ->    if turn == SheepsTurn then iterateGameLoop board positions
                                else sheepMove board positions

