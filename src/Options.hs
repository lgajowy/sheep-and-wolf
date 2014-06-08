module Options where

import IngameDialogs
import System.Exit
import System.IO
import Utils

-- module storing functions for additional options not related directly with playing 

saveGame figuresPostions = do  
    path <- getPathFromUser
    save figuresPostions path
    putStrLn gameSavedMsg

loadGame = do 
    path <- getPathFromUser 
    file <- load path
    return file
    
exitGame = do
    putStrLn exitMsg
    exitSuccess

getPathFromUser = do 
    putStrLn enterPathMsg
    filePath <- getLine
    return filePath


load :: String -> IO FiguresPositions
load databaseFile =
    withFile databaseFile ReadMode (\handle -> do
        contents <- hGetContents handle
        readIO contents)

save :: FiguresPositions -> FilePath -> IO ()
save figuresPostions filePath = writeFile filePath (show figuresPostions)