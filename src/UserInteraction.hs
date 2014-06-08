module UserInteraction where

import Moves
import Utils
import IngameDialogs


-- module storing functions for interaction with users

-- shows available options to user
displayUserOptions = do
    putStrLn (inGameOptionsMsg)

-- gets horizontal movement direction from user
getWolfLeftRight (x,y) = do
  putStrLn leftRightMsg
  direction <- getLine
  case direction of
      "L" -> return (x - 1, y)
      "l" -> return (x - 1, y)
      "R" -> return (x + 1, y)
      "r" -> return (x + 1, y)
      _ -> getWolfLeftRight (x,y)

-- gets vertical movement direction from user
getWolfUpDown (x,y) = do
  putStrLn upDownMsg
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
        putStrLn invalidMoveMsg
        getWolfMovementDirectionFromUser pos pawnPositions

-- prompts user for starting wolf position and lets him choose it
chooseWolfStartingPosition = do
    putStrLn wolfStartingPosMsg
    chosenPosition <- getStartingPositionFromUser
    return chosenPosition

-- gets from standard IO the wolf pawn position
getStartingPositionFromUser :: IO (Int, Int)
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
