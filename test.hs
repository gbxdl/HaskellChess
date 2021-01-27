module Test
( Color
) where

data Color = Black | White deriving (Eq)

isBlack :: Color -> Bool
isBlack color 
 | color == Black = True
 | otherwise = False
