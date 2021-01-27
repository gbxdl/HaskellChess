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

-- instance Read Color where
--    readsPrec _ c = if c=="w" then [(White, "")] else [(Black, "")]

-- instance Read Bool where
--   readPrec =
--     parens
--     ( do L.Ident s <- lexP
--          case s of
--            "True"  -> return True
--            "False" -> return False
--            _       -> pfail
-- )

instance Read Color where
   readsPrec _ "w" = [(White,"")]
   readsPrec _ "b" = [(Black,"")]
   readsPrec _ _ = []

-- readColor :: String -> Color
-- readColor "w" = White 
-- readColor "b" = Black
   
instance Show Color where
   show White = "w"
   show Black = "b"
-- data Piece = K | Q | R | B | K | P | k | q | 

isBoardPosition :: Square -> Bool
isBoardPosition pos = pos `elem` [(x,y)| x<-[0..7], y<-[0..7]]

pieceFromBoard :: Square -> Board -> Char
pieceFromBoard square board
 | isBoardPosition square = board !! snd square !! fst square
 | otherwise = 'x'