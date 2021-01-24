module InterpretFen
( wordsWhen
, notationToMove
, squareToNotation
, fenToBoard
, boardToFen
, fenToColor
, colorToFen
, fenToCastlingRights
, castlingRightsToFen
, fenToEnpassant
, enpassantToFen
, fenToHalfMove
, halfMoveToFen
, fenToFullMove
, fullMoveToFen
, realToCustomFen
, customToRealFen
) where

import Data.Char
import Data.List
import Types

--crashes for empy imput. Should word wtih pattern matching
notationToMove :: String -> Move
notationToMove input 
   | input == "" = ((0,0),(0,0))
   | input == "-" = ((0,0),(0,0))
   | otherwise = let from = head $ words input
                     to = last $ words input
                     (fx:fy:_) = from
                     (tx:ty:_) = to
    in ((ord fx - ord 'a', ord fy - ord '1'),(ord tx - ord 'a', ord ty - ord '1'))
    
squareToNotation :: Square -> String
squareToNotation sqr = chr (fst sqr + ord 'a') : chr (snd sqr + ord '1') : []

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

fenToBoard :: Fen -> [String]
fenToBoard = (wordsWhen (=='/')).head.words

boardToFen :: Board -> Fen
boardToFen = concat.(intersperse "/")

fenToColor :: Fen -> Color
fenToColor fen = words fen !! 1

colorToFen :: Color -> Fen
colorToFen c = c

fenToCastlingRights :: Fen -> CastlingRights
fenToCastlingRights fen = words fen !! 2

castlingRightsToFen :: CastlingRights -> Fen
castlingRightsToFen cr = cr

fenToEnpassant :: Fen -> Enpassant
fenToEnpassant fen = words fen !! 3

enpassantToFen :: Enpassant -> Fen
enpassantToFen enp = enp

fenToHalfMove :: Fen -> HalfMove
fenToHalfMove fen = read (words fen !! 4) :: Int

halfMoveToFen :: HalfMove -> Fen
halfMoveToFen hm = show hm

fenToFullMove :: Fen -> FullMove
fenToFullMove fen = read (words fen !! 5) :: Int

fullMoveToFen :: FullMove -> Fen
fullMoveToFen fm = show fm

realToCustomFen :: Fen -> Fen
realToCustomFen fen = foldr (\x acc -> realToCustomFenChar x ++ acc) "" (head $ words fen) ++ " " ++ (unwords $ tail $ words fen)

realToCustomFenChar :: Char -> String
realToCustomFenChar x 
 | isNumber x = replicate (digitToInt x) '.'
 | otherwise = [x]

customToRealFen :: Fen -> Fen
customToRealFen fen = concat $ foldr (\x acc -> if '.' `elem` x then show (length x) : acc else x : acc) [] $ wordsWhenDot fen
   
wordsWhenDot :: String -> [String]
wordsWhenDot s = case dropWhile (=='.') s of
                      "" -> []
                      s' -> w : wordsWhenNotDot s''
                            where (w, s'') = break (=='.') s'

wordsWhenNotDot :: String -> [String]
wordsWhenNotDot s = case dropWhile (/='.') s of
                      "" -> []
                      s' -> w : wordsWhenDot s''
                            where (w, s'') = break (/='.') s'