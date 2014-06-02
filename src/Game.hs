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
    position <- getWolfMovementDirectionFromUser
    initalWolfPos <- return position
    pawnPos <- return  (initalWolfPos : initialSheepPositions)
    return pawnPos 
     

gameLoop gameBoard pawnPositions = do 
    displayOptionsAndBoard gameBoard
    option <- getLine
    inGameExecuteOption option gameBoard    --TODO how to move from inGameExecution? return coordinates from this function??
    newWolfPosition <- getWolfMovementDirectionFromUser
    wolfMove gameBoard pawnPositions newWolfPosition


getWolfMovementDirectionFromUser = do
    putStrLn "please insert wolf position"
    position <- getLine
    return (read(position) :: Position)

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


validateWolfPosition newPosition (wolf:sheep) = (newPosition:sheep) `elem` possibleWolfMoves (wolf:sheep)


getNewSheepPositions oldPositions = do
    return (chooseMove oldPositions)


displayOptionsAndBoard board = do
    printBoard board
    putStrLn (optionsMsg ++ moveOptionMsg)