module UI (
showFEN
) where

import Data.Char

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

fenToRows :: String -> [String]
fenToRows = (wordsWhen (=='/')).head.words

showRow :: String -> String
showRow row = foldr whatToPrint "" row

whatToPrint :: Char -> String -> String
whatToPrint x acc
    | isNumber x = replicate (ord x - ord '0') ' ' ++ acc
    | otherwise = x:' ':acc

showFEN :: String -> String
showFEN fen = foldl (\acc x -> showRow x ++ "\n" ++ acc) "" $ fenToRows fen
