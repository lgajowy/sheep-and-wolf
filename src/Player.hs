module Player where

class Player a where
    next :: a -> a
    
data WSPlayer = WolfPlayer | SheepPlayer deriving (Show,Read)
instance Player WSPlayer where
    next WolfPlayer    = SheepPlayer
    next SheepPlayer   = WolfPlayer