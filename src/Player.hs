module Player where

-- module storing types of players

class Player a where
    next :: a -> a
    
data WSPlayer = WolfPlayer | SheepPlayer deriving (Show,Read)
instance Player WSPlayer where
    next WolfPlayer    = SheepPlayer
    next SheepPlayer   = WolfPlayer