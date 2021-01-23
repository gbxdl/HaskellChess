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
, isBoardPosition
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

isBoardPosition :: Square -> Bool
isBoardPosition pos = pos `elem` [(x,y)| x<-[0..7], y<-[0..7]]

pieceFromBoard :: Square -> Board -> Char
pieceFromBoard square board
 | isBoardPosition square = board !! snd square !! fst square
 | otherwise = 'x'