module Game where

import Board
import Piece
import Player
import IngameDialogs
import IngameOptions

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
                [Square Nothing, EmptySquare, Square Nothing, Square(Just Wolf), Square Nothing, EmptySquare, Square Nothing, EmptySquare]
               ]

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
    gameLoop gameBoard

gameLoop gameBoard = do
    displayOptionsAndBoard gameBoard
    option <- getLine
    inGameExecuteOption option gameBoard

    -- sheep moves
    -- updateMatrixAt
    -- displayBoard gameBoard
    -- check verdict if game lost/won display message gameloop otherwise!

    gameLoop gameBoard

displayOptionsAndBoard board = do
    printBoard board
    putStrLn (optionsMsg ++ moveOptionMsg)


