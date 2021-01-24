module UI (
showFen
) where

import Types
import InterpretFen
import Data.Char

showFen :: Fen -> String
showFen fen = "\n" ++ foldl (\acc x -> foldr (\x acc -> x : acc) "" x ++ "\n" ++ acc) "" (fenToBoard fen)