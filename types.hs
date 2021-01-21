module Types
( Board
, Move
, Row
, Square
, Fen
, Fens
, Color
, Enpassant
, CastlingRights
, HalfMove
, FullMove
, pieceFromBoard
) where

type Fen = String
type Fens = [String]
type Board = [String]
type Row = String

type Square = (Int, Int)
type Move = (Square,Square)

type Color = String
type Enpassant = String
type CastlingRights = String
type HalfMove = Int
type FullMove = Int

-- data Color = Color "W" | "B" -- deriving (show)
-- data Piece = K | Q | R | B | K | P | k | q | 

pieceFromBoard :: Board -> Square -> Char
pieceFromBoard board square = board !! snd square !! fst square