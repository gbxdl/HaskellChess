module UI (
showFen
) where

import Types
import InterpretFen

showFen :: Fen -> String
showFen fen = "\n" ++ foldl (\acc x -> x++ "\n" ++ acc) "" (fenToBoard fen)