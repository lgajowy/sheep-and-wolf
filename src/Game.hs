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

run = do
    putStrLn welcomeMsg
    mainProgramLoop

mainProgramLoop = do
    putStrLn optionsMsg
    option <- getLine
    executeOption option initialBoard

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
        "1" -> startGame initialBoard
        "2" -> saveGame gameBoard
        "3" -> loadGame
        "4" -> exitGame
        "5" -> putStrLn "wolf moves\n"
        _ -> do
            putStrLn wrongOptionMsg
            option <- getLine
            inGameExecuteOption option gameBoard



startGame gameBoard = do
    startingPawnPositions <- chooseWolfStartingPosition    
    startingBoard <- moveWolfOnBoard (2,7) (head startingPawnPositions) gameBoard
    gameLoop startingBoard startingPawnPositions


chooseWolfStartingPosition = do
    putStrLn wolfStartingPosMsg  

    choosenPosition <- getStartingPositionFromUser
    pawnPos <- return  ((0, 7) : initialSheepPositions)
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


gameLoop gameBoard pawnPositions = do
    displayOptionsAndBoard gameBoard
    option <- getLine
    inGameExecuteOption option gameBoard
    newWolfPosition <- getWolfMovementDirectionFromUser (head pawnPositions) pawnPositions
    wolfMove gameBoard pawnPositions newWolfPosition

getWolfLeftRight (x,y) = do
  putStrLn "(L)eft/(R)ight?"
  direction <- getLine
  case direction of
      "L" -> return (x - 1, y)
      "l" -> return (x - 1, y)
      "R" -> return (x + 1, y)
      "r" -> return (x + 1, y)
      _ -> getWolfLeftRight (x,y)


getWolfUpDown (x,y) = do
  putStrLn "(U)p/(D)own?"
  direction <- getLine
  case direction of
      "U" -> getWolfLeftRight (x, y - 1)
      "u" -> getWolfLeftRight (x, y - 1)
      "D" -> getWolfLeftRight (x, y + 1)
      "d" -> getWolfLeftRight (x, y + 1)
      _ -> getWolfUpDown (x,y)

getWolfMovementDirectionFromUser pos pawnPositions = do
    position <- getWolfUpDown pos
    if validateWolfPosition position pawnPositions then
      return position
    else
      do
        putStrLn "Invalid move"
        getWolfMovementDirectionFromUser pos pawnPositions

wolfMove gameBoard pawnPositions moveCoordinates = do
     board <- moveWolfOnBoard (head pawnPositions) moveCoordinates gameBoard
     printBoard board
     newPawnPositions <- return (moveCoordinates : (tail pawnPositions))
     checkVerdict board newPawnPositions WolfTurn


sheepMove gameBoard pawnPositions = do
    positions <- getNewSheepPositions (pawnPositions)
    board <- moveSheepOnBoard gameBoard (pawnPositions) positions
    printBoard board
    checkVerdict board positions SheepsTurn


checkVerdict board positions turn = case verdict positions turn of
    WolfWon     ->    putStrLn wolfWonMsg
    SheepsWon   ->    putStrLn sheepWonMsg
    NotEnd      ->    if turn == SheepsTurn then gameLoop board positions
                                else sheepMove board positions


getNewSheepPositions oldPositions = do
    return (chooseMove oldPositions)


displayOptionsAndBoard board = do
    printBoard board
    putStrLn (optionsMsg ++ moveOptionMsg)
