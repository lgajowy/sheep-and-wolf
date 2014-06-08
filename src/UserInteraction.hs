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