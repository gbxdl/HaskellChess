module Rules(
isLegalMove
) where

import Types
import InterpretFEN

isLegalMove :: Move -> FEN -> Bool
isLegalMove move pos = True

