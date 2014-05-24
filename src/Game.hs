module Game where

import Board
import Piece
import Player
import IngameDialogs

import System.Exit

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

mainGameLoop = do 
    putStrLn welcomeMsg
    putStrLn optionsMsg
    option <- getLine
    executeOption option
    mainGameLoop

executeOption option = case option of
        "1" -> startGame
        "2" -> saveGame
        "3" -> loadGame
        "4" -> exitGame
        _ -> do
            putStrLn wrongOptionMsg
            option <- getLine
            executeOption option

getPathFromUser = do 
    putStrLn enterPathMsg
    filePath <- getLine
    return filePath

startGame = do 
    putStrLn "game started"

saveGame = do  
    path <- getPathFromUser
    save initialBoard path
    putStrLn gameSavedMsg

loadGame = do 
    path <- getPathFromUser     -- TODO! How to load game??
    file <- load path
    putStrLn file
    
exitGame = do
    putStrLn exitMsg
    exitSuccess


load :: (Read a) => FilePath -> IO a
load f = do s <- readFile f
            return (read s)

save :: Board -> FilePath -> IO ()
save board filePath = writeFile filePath (show board)
