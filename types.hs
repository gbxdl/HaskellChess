module Types
( Position
, Move
, Square
, FEN
) where

type FEN = String
type Position = [String]
type Square = (Int, Int)
type Move = (Square,Square)
