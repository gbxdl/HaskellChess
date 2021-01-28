module Types
( Board
, Move
, Row
, Square
, Fen
, Fens
, Color (..)
, Enpassant
, CastlingRights
, HalfMove
, FullMove
, pieceFromBoard
, isBoardPosition
) where

type Fen = String
type Fens = [String]
type Board = [String]
type Row = String

type Square = (Int, Int)
type Move = (Square,Square)

type Enpassant = String
type CastlingRights = String
type HalfMove = Int
type FullMove = Int

data Color = White | Black deriving (Eq)
data Piece = King Color | Queen Color| Rook Color | Bishop Color | Knight Color | Pawn Color | Empty deriving (Eq) 
-- data Enpassant = 
instance Read Color where
   readsPrec _ "w" = [(White,"")]
   readsPrec _ "b" = [(Black,"")]
   readsPrec _ _ = []

instance Show Color where
   show White = "w"
   show Black = "b"

instance Show Piece where
   show (King color) =  case color of
                           White -> "K"
                           Black -> "k"
   show (Queen color) = case color of
                           White -> "Q"
                           Black -> "q"
   show (Rook color) = case color of
                           White -> "R"
                           Black -> "r"
   show (Bishop color) = case color of
                           White -> "B"
                           Black -> "b"
   show (Knight color) = case color of
                           White -> "N"
                           Black -> "n"
   show (Pawn color) = case color of
                           White -> "P"
                           Black -> "p"
   show Empty = "."

instance Read Piece where
   readsPrec _ "K" = [(King White,"")]
   readsPrec _ "k" = [(King White,"")]
   readsPrec _ "Q" = [(Queen White,"")]
   readsPrec _ "q" = [(Queen Black,"")]
   readsPrec _ "R" = [(Rook White,"")]
   readsPrec _ "r" = [(Rook Black,"")]
   readsPrec _ "B" = [(Bishop White,"")]
   readsPrec _ "b" = [(Bishop Black,"")]
   readsPrec _ "N" = [(Knight White,"")]
   readsPrec _ "n" = [(Knight Black,"")]
   readsPrec _ "P" = [(Pawn White,"")]
   readsPrec _ "p" = [(Pawn Black,"")]
   readsPrec _ "." = [(Empty,"")]
   readsPrec _ _ = []
   
isBoardPosition :: Square -> Bool
isBoardPosition pos = pos `elem` [(x,y)| x<-[0..7], y<-[0..7]]

pieceFromBoard :: Square -> Board -> Char
pieceFromBoard square board
 | isBoardPosition square = board !! snd square !! fst square
 | otherwise = 'x'