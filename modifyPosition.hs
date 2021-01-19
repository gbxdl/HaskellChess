module ModifyPosition(
startingPosition,
nextPosition
) where

import Types

startingPosition :: FEN
startingPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

nextPosition :: Move -> FEN -> FEN
nextPosition move pos = pos
