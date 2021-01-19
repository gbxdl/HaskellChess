module InterpretFEN
( wordsWhen
, fenToRows
, showRow
) where

import Data.Char  
import Types

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

fenToRows :: FEN -> [String]
fenToRows = (wordsWhen (=='/')).head.words

showRow :: String -> String
showRow row = foldr whatToPrint "" row

whatToPrint :: Char -> String -> String
whatToPrint x acc
    | isNumber x = replicate (ord x - ord '0') ' ' ++ acc
    | otherwise = x:' ':acc

fenToArray :: FEN -> String -- should still become a list of strings. Make fold word one level deeper mapM or so.
fenToArray fen = foldl (\acc x -> showRow x ++ acc) "" $ fenToRows fen
