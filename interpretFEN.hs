module InterpretFen
( wordsWhen
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
) where

import Data.Char
import Data.List
import Types

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