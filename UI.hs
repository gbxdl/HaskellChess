module UI (
showFEN
) where

import Types
import InterpretFEN

showFEN :: FEN -> String
showFEN fen = "\n" ++ foldl (\acc x -> showRow x ++ "\n" ++ acc) "" (fenToRows fen)