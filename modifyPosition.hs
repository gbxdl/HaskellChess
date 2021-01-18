module ModifyPosition(
startingPosition,
nextPosition
) where

import Types

startingPosition :: Position
startingPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

nextPosition :: Move -> Position -> Position
nextPosition move pos = pos
