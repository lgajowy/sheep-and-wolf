module IngameOptions where

import IngameDialogs
import System.Exit
import Board

-- module storing functions for additional options not related directly with playing 

saveGame board = do  
    path <- getPathFromUser
    save board path
    putStrLn gameSavedMsg

loadGame = do 
    path <- getPathFromUser 
    file <- load path
    putStrLn file
    
exitGame = do
    putStrLn exitMsg
    exitSuccess

getPathFromUser = do 
    putStrLn enterPathMsg
    filePath <- getLine
    return filePath

load :: (Read a) => FilePath -> IO a
load f = do s <- readFile f
            return (read s)

save :: Board -> FilePath -> IO ()
save board filePath = writeFile filePath (show board)