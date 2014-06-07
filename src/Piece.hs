module Piece where

-- module storing types of pieces (Sheep and Wolf)

data Piece = Sheep | Wolf deriving Eq

instance Show Piece where 
    show Sheep = "S"
    show Wolf = "W"

instance Read Piece where
    readsPrec _ ('W':xs)   = [(Wolf, xs)]
    readsPrec _ ('S':xs)   = [(Sheep, xs)]