module UI (
showFen
) where

import Types
import InterpretFen
import Data.Char

showFen :: Fen -> String
showFen fen = "\n" ++ foldl (\acc x -> foldr (\x acc -> flipCapitals x : acc) "" x ++ "\n" ++ acc) "" (fenToBoard fen)

flipCapitals :: Char -> Char
flipCapitals x
 | isUpper x = toLower x
 | isLower x = toUpper x
 | otherwise = x